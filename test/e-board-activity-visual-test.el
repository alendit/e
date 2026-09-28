;;; e-board-activity-visual-test.el --- Visual Board activity tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-board-activity-visual-shell)

(ert-deftest e-board-activity-visual-test-unavailable-without-native-xwidget ()
  "The Lisp WebKit command does not imply native xwidget support."
  (let ((native-featurep (symbol-function 'featurep)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &optional subfeature)
                 (and (not (eq feature 'xwidget-internal))
                      (funcall native-featurep feature subfeature)))))
      (should (equal (e-board-activity-visual-unavailable-reason)
                     "Emacs has no xwidget-webkit support")))))

(defun e-board-activity-visual-test--control-payload
    (action &optional revision attempt participant-id &rest fields)
  "Return a task control payload for ACTION and optional coordinates."
  (copy-tree
   (append
    `((action . ,action)
      (boardId . "board-1") (runSetEpoch . 4)
      (generation . 3) (revision . ,(or revision 10))
      (runId . "run-1") (taskKey . "task")
      (attempt . ,(or attempt 2))
      (participantId . ,(or participant-id "worker-1")))
    fields)))

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
    (should (eq (alist-get 'browseAvailable run-set) t))
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
  "A manifest sentinel enables browsing without inflating counts."
  (let* ((projection '(:status running :restore-state ready :ready-p t
                       :active-run-count 32 :omitted-count 0 :more-p t))
         (run-set (e-board-activity-visual-view-model-run-set projection))
         (expanded
          (e-board-activity-visual-view-model-run-set projection t)))
    (should (eq (alist-get 'browseAvailable run-set) t))
    (should (= (alist-get 'activeCount run-set) 32))
    (should (= (alist-get 'omittedCount run-set) 0))
    (should (eq (alist-get 'moreMayExist expanded) t))
    (should-not (eq (alist-get 'browseAvailable expanded) t))
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

(ert-deftest e-board-activity-visual-test-task-controls-follow-exact-live-capability ()
  "Task controls require its exact admitted row and active live capability."
  (let* ((row '(:participant-id "worker-1" :run-id "run-1"
                :task-key "task" :attempt 2))
         (task (list :task-key "task" :accepted-attempt 2
                     :participant-id "worker-1" :participant-row row))
         (record '(:board-id "board-1" :participant-id "worker-1"
                   :session-id "worker-1" :run-id "run-1"
                   :task-key "task" :attempt 2))
         (work
          (e-work-prepare
           (e-work-spec-create
            :id "board-visual-controls"
            :execution 'cheap :interactive-policy 'cheap
            :runner (lambda (_arguments _context) nil))
           nil))
         (entry (list :board-id "board-1" :participant-id "worker-1"
                      :harness 'worker-harness :work-handle work
                      :callbacks (list :record (lambda () record))))
         (live-entry entry))
    (cl-letf (((symbol-function 'e-subagent-live-get)
               (lambda (owner board-id participant-id)
                 (when (and (eq owner 'live)
                            (equal board-id "board-1")
                            (equal participant-id "worker-1"))
                   live-entry)))
              )
      (let ((controls
             (e-board-activity-visual-view-model-task-controls
              "board-1" "run-1" task 'live)))
        (dolist (name '(canOpenChat canSteer canSend canInterrupt canShutdown))
          (should (eq (alist-get name controls) t))))
      (setq live-entry nil)
      (let ((controls
             (e-board-activity-visual-view-model-task-controls
              "board-1" "run-1" task 'live)))
        (should (eq (alist-get 'canOpenChat controls) t))
        (dolist (name '(canSteer canSend canInterrupt canShutdown))
          (should (eq (alist-get name controls) :json-false))))
      (setq live-entry entry
            record (plist-put record :attempt 1))
      (let ((controls
             (e-board-activity-visual-view-model-task-controls
              "board-1" "run-1" task 'live)))
        (should (eq (alist-get 'canOpenChat controls) t))
        (dolist (name '(canSteer canSend canInterrupt canShutdown))
          (should (eq (alist-get name controls) :json-false))))
      (setq record (plist-put record :attempt 2))
      (e-work-finish work 'done)
      (let ((controls
             (e-board-activity-visual-view-model-task-controls
              "board-1" "run-1" task 'live)))
        (should (eq (alist-get 'canOpenChat controls) t))
        (dolist (name '(canSteer canSend canInterrupt canShutdown))
          (should (eq (alist-get name controls) :json-false)))))))

(ert-deftest e-board-activity-visual-test-task-actions-revalidate-before-dispatch ()
  "Stale and unavailable clicks do nothing; current task actions route once."
  (let* ((target 'target)
         (binding (e-chat-service--binding-create
                   :lifecycle-state 'ready :board-id "board-1"))
         (row '(:participant-id "worker-1" :run-id "run-1"
                :task-key "task" :attempt 2))
         (record '(:board-id "board-1" :participant-id "worker-1"
                   :session-id "worker-1" :run-id "run-1"
                   :task-key "task" :attempt 2))
         (work
          (e-work-prepare
           (e-work-spec-create
            :id "board-visual-task-action"
            :execution 'cheap :interactive-policy 'cheap
            :runner (lambda (_arguments _context) nil))
           nil))
         (entry (list :board-id "board-1" :participant-id "worker-1"
                      :harness 'worker-harness :work-handle work
                      :callbacks (list :record (lambda () record))))
         (live-entry entry)
         (calls nil)
         (messages nil))
    (with-temp-buffer
      (setq-local e-board-activity-visual--target target
                  e-board-activity-visual--binding binding
                  e-board-activity-visual--live 'live
                  e-board-activity-visual--run-set-epoch 4
                  e-board-activity-visual--selected-run-id "run-1"
                  e-board-activity-visual--selected-task
                  '(:run-task "run-1" "task" 2)
                  e-board-activity-visual--detail-state 'ready
                  e-board-activity-visual--detail-page
                  (list :board-id "board-1" :generation 3 :revision 10
                        :run '(:run-id "run-1")
                        :tasks (list
                                (list :task-key "task"
                                      :accepted-attempt 2
                                      :participant-id "worker-1"
                                      :participant-row row))))
      (cl-letf (((symbol-function 'e-board-activity-visual--target-id)
                 (lambda () "board-1"))
                ((symbol-function 'e-board-activity-visual--schedule-push)
                 #'ignore)
                ((symbol-function 'message)
                 (lambda (format-string &rest values)
                   (push (apply #'format format-string values) messages)))
                ((symbol-function 'e-subagent-live-get)
                 (lambda (owner board-id participant-id)
                   (when (and (eq owner 'live)
                              (equal board-id "board-1")
                              (equal participant-id "worker-1"))
                     live-entry)))
                ((symbol-function 'e-board-activity-shell-open-participant-chat)
                 (lambda (actual-row board-id live)
                   (push (list 'open-chat actual-row board-id live) calls)))
                ((symbol-function 'e-subagent-steer)
                 (lambda (&rest arguments)
                   (push (cons 'steer arguments) calls)))
                ((symbol-function 'e-subagent-send)
                 (lambda (&rest arguments)
                   (push (cons 'send arguments) calls)))
                ((symbol-function 'e-subagent-interrupt)
                 (lambda (&rest arguments)
                   (push (cons 'interrupt arguments) calls)))
                ((symbol-function 'e-subagent-shutdown)
                 (lambda (&rest arguments)
                   (push (cons 'shutdown arguments) calls)))
                )
        (e-board-activity-visual--handle-ui-action
         (e-board-activity-visual-test--control-payload
          "steer-participant" 9))
        (e-board-activity-visual--handle-ui-action
         (e-board-activity-visual-test--control-payload
          "send-participant" 10 1))
        (e-board-activity-visual--handle-ui-action
         (e-board-activity-visual-test--control-payload
          "interrupt-participant" 10 2 "other-worker"))
        (should-not calls)
        (setq live-entry nil)
        (e-board-activity-visual--handle-ui-action
         (e-board-activity-visual-test--control-payload "shutdown-participant"))
        (should-not calls)
        ;; Durable admitted participants remain chat-accessible without a
        ;; process-local live control owner.
        (e-board-activity-visual--handle-ui-action
         (e-board-activity-visual-test--control-payload "open-task-participant"))
        (setq live-entry entry)
        (e-board-activity-visual--handle-ui-action
         (e-board-activity-visual-test--control-payload
          "steer-participant" nil nil nil '(prompt . "direct") '(reason . "why")))
        (e-board-activity-visual--handle-ui-action
         (e-board-activity-visual-test--control-payload
          "send-participant" nil nil nil '(prompt . "follow up")))
        (e-board-activity-visual--handle-ui-action
         (e-board-activity-visual-test--control-payload "interrupt-participant"))
        (e-board-activity-visual--handle-ui-action
         (e-board-activity-visual-test--control-payload "shutdown-participant"))
        (should
         (equal (nreverse calls)
                `((open-chat ,row "board-1" live)
                  (steer live "board-1" target "worker-1" "direct" "why")
                  (send live "board-1" "worker-1" "follow up")
                  (interrupt live "board-1" target "worker-1")
                  (shutdown live "board-1" target "worker-1"))))
        (should messages)))))

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

(ert-deftest e-board-activity-visual-test-rebind-never-reuses-old-control-epoch ()
  "An old same-Board control stays stale after rebinding the same buffer."
  (let* ((e-board-activity-visual-buffer-name
          (generate-new-buffer-name "*e-visual-epoch-test*"))
         (e-board-activity-visual--epoch-counter 0)
         (buffer (get-buffer-create e-board-activity-visual-buffer-name))
         (target 'target)
         (first (e-chat-service--binding-create
                 :lifecycle-state 'ready :board-id "board-1"))
         (second (e-chat-service--binding-create
                  :lifecycle-state 'ready :board-id "board-1"))
         (projection '(:projection (:board-id "board-1" :ready-p t
                                   :runs ((:run-id "run-1")))))
         old-epoch old-interrupt old-shutdown calls)
    (unwind-protect
        (cl-letf (((symbol-function 'e-board-activity-visual--ensure-runtime)
                   #'ignore)
                  ((symbol-function
                    'e-board-sqlite-publication-target-valid-p)
                   (lambda (_target) t))
                  ((symbol-function
                    'e-board-sqlite-publication-target-board-id)
                   (lambda (_target) "board-1"))
                  ((symbol-function 'emacs-egui-create-buffer)
                   (lambda (&rest _) (list :buffer buffer)))
                  ((symbol-function 'e-board-activity-visual--wire-actions)
                   #'ignore)
                  ((symbol-function 'e-board-activity-visual--push-snapshot)
                   #'ignore)
                  ((symbol-function 'e-board-activity-visual--schedule-push)
                   #'ignore)
                  ((symbol-function 'e-board-activity-visual--refresh-detail-page)
                   #'ignore)
                  ((symbol-function 'e-board-run-set-subscribe)
                   (lambda (&rest _) #'ignore))
                  ((symbol-function 'e-workspace-pop-to-buffer)
                   #'ignore)
                  ((symbol-function
                    'e-board-activity-visual-view-model-task-controls)
                   (lambda (&rest _)
                     '((canInterrupt . t) (canShutdown . t))))
                  ((symbol-function 'e-subagent-interrupt)
                   (lambda (&rest _) (push 'interrupt calls)))
                  ((symbol-function 'e-subagent-shutdown)
                   (lambda (&rest _) (push 'shutdown calls))))
          (e-board-activity-visual-open-buffer
           :target target :binding first :live 'live :run-id "run-1")
          (e-board-activity-visual--run-set-updated
           buffer target first projection)
          (with-current-buffer buffer
            (setq e-board-activity-visual--selector-browsing t
                  e-board-activity-visual--selector-page nil)
            (e-board-activity-visual--handle-ui-action
             `((action . "current-runs") (boardId . "board-1")
               (runSetEpoch . ,e-board-activity-visual--run-set-epoch)))
            (setq old-epoch e-board-activity-visual--run-set-epoch
                  old-interrupt
                  (e-board-activity-visual-test--control-payload
                   "interrupt-participant")
                  old-shutdown
                  (e-board-activity-visual-test--control-payload
                   "shutdown-participant"))
            (setf (alist-get 'runSetEpoch old-interrupt) old-epoch
                  (alist-get 'runSetEpoch old-shutdown) old-epoch))
          (e-board-activity-visual-open-buffer
           :target target :binding second :live 'live :run-id "run-1")
          (e-board-activity-visual--run-set-updated
           buffer target second projection)
          (with-current-buffer buffer
            (should (> e-board-activity-visual--run-set-epoch old-epoch))
            (setq e-board-activity-visual--selected-task
                  '(:run-task "run-1" "task" 2)
                  e-board-activity-visual--detail-state 'ready
                  e-board-activity-visual--detail-page
                  '(:board-id "board-1" :generation 3 :revision 10
                    :run (:run-id "run-1")
                    :tasks ((:task-key "task" :accepted-attempt 2
                             :participant-id "worker-1"
                             :participant-row
                             (:participant-id "worker-1" :run-id "run-1"
                              :task-key "task" :attempt 2)))))
            (e-board-activity-visual--handle-ui-action old-interrupt)
            (e-board-activity-visual--handle-ui-action old-shutdown)
            (should-not calls)
            (setf (alist-get 'runSetEpoch old-interrupt)
                  e-board-activity-visual--run-set-epoch
                  (alist-get 'runSetEpoch old-shutdown)
                  e-board-activity-visual--run-set-epoch)
            (e-board-activity-visual--handle-ui-action old-interrupt)
            (e-board-activity-visual--handle-ui-action old-shutdown)
            (should (equal (nreverse calls) '(interrupt shutdown)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-board-activity-visual-test-browse-accepts-manifest-sentinel ()
  "Browse is accepted when the bounded query found another manifest."
  (let ((e-board-activity-visual--binding
         (e-chat-service--binding-create
          :lifecycle-state 'ready :board-id "board-1"))
        (e-board-activity-visual--run-set-epoch 4)
        (e-board-activity-visual--run-set-projection
         '(:active-run-count 32 :omitted-count 0 :more-p t))
        started)
    (cl-letf (((symbol-function 'e-board-activity-visual--target-id)
               (lambda () "board-1"))
              ((symbol-function 'e-board-activity-visual--start-selector-page)
               (lambda (cursor) (should-not cursor) (setq started t)))
              ((symbol-function 'e-board-activity-visual--schedule-push)
               #'ignore))
      (e-board-activity-visual--handle-ui-action
       '((action . "browse-runs") (boardId . "board-1")
         (runSetEpoch . 4)))
      (should started))))

(ert-deftest e-board-activity-visual-test-browses-past-256-with-one-page ()
  "Indexed browsing reaches run 257 while retaining one bounded page."
  (let ((target 'target)
        (binding (e-chat-service--binding-create
                  :lifecycle-state 'ready :board-id "board-1"))
        (queries 0))
    (with-temp-buffer
      (setq-local e-board-activity-visual--target target
                  e-board-activity-visual--binding binding
                  e-board-activity-visual--run-set-epoch 4
                  e-board-activity-visual--run-set-projection
                  '(:board-id "board-1" :ready-p t :restore-state ready
                    :active-run-count 257 :more-p t
                    :runs ((:run-id "run-000")))
                  e-board-activity-visual--selected-run-id "run-000")
      (cl-letf (((symbol-function 'e-board-activity-visual--target-id)
                 (lambda () "board-1"))
                ((symbol-function 'e-board-activity-visual--schedule-push)
                 #'ignore)
                ((symbol-function
                  'e-board-sqlite-publication-target-orchestration-run-index-page-start)
                 (lambda (_target &rest args)
                   (cl-incf queries)
                   (should (= (plist-get args :limit) 64))
                   (should (eq (plist-get args :active-only) t))
                   (let* ((cursor (plist-get args :cursor))
                          (start (or (plist-get cursor :start) 0))
                          (end (min 257 (+ start 64)))
                          (next (when (< end 257)
                                  (list :generation 3 :start end)))
                          (entries
                           (cl-loop for index from start below end
                                    collect
                                    (list :summary
                                          (list :run-id
                                                (format "run-%03d" index)
                                                :label
                                                (format "Run %d" index))))))
                     (e-work-start
                      (e-work-spec-create
                       :id "visual-index-page" :execution 'cheap
                       :interactive-policy 'cheap
                       :runner
                       (lambda (_arguments _context)
                         (list :board-id "board-1" :generation 3
                               :cursor cursor :next-cursor next
                               :entries entries)))
                      nil)))))
        (e-board-activity-visual--start-selector-page nil)
        (dotimes (_ 4)
          (let* ((snapshot (e-board-activity-visual--snapshot))
                 (run-set (alist-get 'runSet snapshot)))
            (should (<= (length (alist-get 'runs run-set)) 64))
            (should (eq (alist-get 'nextAvailable run-set) t))
            (e-board-activity-visual--handle-ui-action
             `((action . "next-runs") (boardId . "board-1")
               (runSetEpoch . ,e-board-activity-visual--run-set-epoch)
               (pageGeneration . 3)))))
        (let* ((snapshot (e-board-activity-visual--snapshot))
               (run-set (alist-get 'runSet snapshot))
               (runs (alist-get 'runs run-set)))
          (should (= queries 5))
          (should (= (length runs) 1))
          (should (equal (alist-get 'runId (aref runs 0)) "run-256"))
          (should-not (eq (alist-get 'nextAvailable run-set) t))
          (should-not (eq (alist-get 'selectedRunVisible run-set) t))
          (should (equal e-board-activity-visual--selected-run-id "run-000"))
          (should (equal (plist-get
                          e-board-activity-visual--run-set-projection
                          :runs)
                         '((:run-id "run-000"))))
          (e-board-activity-visual--handle-ui-action
           `((action . "current-runs") (boardId . "board-1")
             (runSetEpoch . ,e-board-activity-visual--run-set-epoch)
             (pageGeneration . 3)))
          (should-not e-board-activity-visual--selector-browsing)
          (should (equal (alist-get 'runId
                                    (aref (alist-get
                                           'runs
                                           (alist-get 'runSet
                                                      (e-board-activity-visual--snapshot)))
                                          0))
                         "run-000")))))))

(ert-deftest e-board-activity-visual-test-selector-fences-page-and-actions ()
  "Late page responses and stale page clicks cannot change selection."
  (let ((target 'target)
        (binding (e-chat-service--binding-create
                  :lifecycle-state 'ready :board-id "board-1"))
        (other-binding (e-chat-service--binding-create
                        :lifecycle-state 'ready :board-id "board-1"))
        (page '(:board-id "board-1" :generation 3 :cursor nil
                :entries ((:summary (:run-id "run-2")))))
        (settled
         (e-work-start
          (e-work-spec-create
           :id "visual-selector-settlement" :execution 'cheap
           :interactive-policy 'cheap
           :runner
           (lambda (_arguments _context)
             '(:board-id "board-1" :generation 9 :cursor nil
               :entries ((:summary (:run-id "stale"))))))
          nil)))
    (with-temp-buffer
      (setq-local e-board-activity-visual--target target
                  e-board-activity-visual--binding binding
                  e-board-activity-visual--run-set-epoch 8
                  e-board-activity-visual--selector-browsing t
                  e-board-activity-visual--selector-page page
                  e-board-activity-visual--selector-work 'pending
                  e-board-activity-visual--selected-run-id "run-1")
      (cl-letf (((symbol-function 'e-board-activity-visual--target-id)
                 (lambda () "board-1"))
                ((symbol-function 'e-board-activity-visual--schedule-push)
                 #'ignore)
                ((symbol-function 'e-board-activity-visual--refresh-detail-page)
                 #'ignore))
        (dolist (coordinates
                 (list (list (current-buffer) 'other-target binding 8)
                       (list (current-buffer) target other-binding 8)
                       (list (current-buffer) target binding 7)))
          (apply #'e-board-activity-visual--selector-page-settled
                 (append coordinates (list nil 'pending settled)))
          (should (eq e-board-activity-visual--selector-page page)))
        (e-board-activity-visual--handle-ui-action
         '((action . "select-run") (boardId . "board-1")
           (runSetEpoch . 8) (pageGeneration . 2) (runId . "run-2")))
        (should (equal e-board-activity-visual--selected-run-id "run-1"))
        (e-board-activity-visual--handle-ui-action
         '((action . "select-run") (boardId . "board-1")
           (runSetEpoch . 7) (pageGeneration . 3) (runId . "run-2")))
        (should (equal e-board-activity-visual--selected-run-id "run-1"))
        (e-board-activity-visual--handle-ui-action
         '((action . "select-run") (boardId . "board-1")
           (runSetEpoch . 8) (pageGeneration . 3) (runId . "run-2")))
        (should (equal e-board-activity-visual--selected-run-id "run-2"))))))

(ert-deftest e-board-activity-visual-test-stale-cursor-restarts-first-page ()
  "A cleared Board's rejected cursor returns browsing to page one."
  (let* ((target 'target)
         (binding (e-chat-service--binding-create
                   :lifecycle-state 'ready :board-id "board-1"))
         (cursor '(:generation 3 :after-position 64))
         (failure
          (e-work-start
           (e-work-spec-create
            :id "stale-visual-cursor" :execution 'cheap
            :interactive-policy 'cheap
            :runner
            (lambda (_arguments _context)
              (error "Board run-index cursor is invalid or stale")))
           nil))
         restarted)
    (with-temp-buffer
      (setq-local e-board-activity-visual--target target
                  e-board-activity-visual--binding binding
                  e-board-activity-visual--run-set-epoch 8
                  e-board-activity-visual--selector-browsing t
                  e-board-activity-visual--selector-cursor cursor
                  e-board-activity-visual--selector-work failure)
      (cl-letf (((symbol-function 'e-board-activity-visual--target-id)
                 (lambda () "board-1"))
                ((symbol-function 'e-board-activity-visual--start-selector-page)
                 (lambda (next) (setq restarted (eq next nil))))
                ((symbol-function 'e-board-activity-visual--schedule-push)
                 #'ignore))
        (e-board-activity-visual--selector-page-settled
         (current-buffer) target binding 8 cursor failure failure)
        (should restarted)))))

(ert-deftest e-board-activity-visual-test-browsing-notification-refreshes-detail ()
  "A browsing notification refreshes selected detail even outside the page."
  (let* ((target 'target)
         (binding (e-chat-service--binding-create
                   :lifecycle-state 'ready :board-id "board-1"))
         (selected-task '(:run-task "run-1" "task" 2))
         request-args settle browsing-started cancelled)
    (with-temp-buffer
      (setq-local e-board-activity-visual--target target
                  e-board-activity-visual--binding binding
                  e-board-activity-visual--run-set-epoch 4
                  e-board-activity-visual--selector-browsing t
                  e-board-activity-visual--selector-work 'stale-run-set
                  e-board-activity-visual--detail-request 'stale-detail
                  e-board-activity-visual--selected-run-id "run-1"
                  e-board-activity-visual--selected-task selected-task
                  e-board-activity-visual--detail-state 'ready
                  e-board-activity-visual--detail-page
                  '(:board-id "board-1" :generation 2 :revision 11
                    :run (:run-id "run-1")
                    :tasks ((:task-key "task" :accepted-attempt 2))))
      (cl-letf (((symbol-function
                  'e-board-sqlite-publication-target-valid-p)
                 (lambda (_target) t))
                ((symbol-function
                  'e-board-sqlite-publication-target-board-id)
                 (lambda (_target) "board-1"))
                ((symbol-function 'e-board-activity-visual--target-id)
                 (lambda () "board-1"))
                ((symbol-function 'e-board-activity-visual--cancel-work)
                 (lambda (work)
                   (when work (push work cancelled))))
                ((symbol-function 'e-board-observation-activity-page-start)
                 (lambda (actual-target &rest args)
                   (setq request-args (cons actual-target args))
                   'fresh-detail))
                ((symbol-function 'e-work-on-settle)
                 (lambda (work callback)
                   (should (eq work 'fresh-detail))
                   (setq settle callback)))
                ((symbol-function
                  'e-board-activity-visual--start-selector-page)
                 (lambda (cursor) (should-not cursor)
                   (setq browsing-started t)))
                ((symbol-function 'e-board-activity-visual--schedule-push)
                 #'ignore))
        (e-board-activity-visual--run-set-updated
         (current-buffer) target binding
         '(:projection (:board-id "board-1"
                       :runs ((:run-id "run-2")))))
        (should browsing-started)
        (should (equal (car request-args) target))
        (should (equal (plist-get (cdr request-args) :run-id) "run-1"))
        (should (member 'stale-detail cancelled))
        (should (> e-board-activity-visual--run-set-epoch 4))
        (should (eq e-board-activity-visual--detail-request 'fresh-detail))
        (should (eq e-board-activity-visual--detail-state 'loading))
        (should (equal e-board-activity-visual--selected-run-id "run-1"))
        (should (equal e-board-activity-visual--selected-task selected-task))
        ;; A superseded page query cannot replace the newer run-set.
        (e-board-activity-visual--selector-page-settled
         (current-buffer) target binding 4 nil 'stale-run-set nil)
        (should (equal (plist-get e-board-activity-visual--run-set-projection
                                  :board-id)
                       "board-1"))
        ;; A cancelled request cannot replace the newly requested page.
        (e-board-activity-visual--detail-page-settled
         (current-buffer) target "run-1" nil 'stale-detail nil)
        (should (eq e-board-activity-visual--detail-request 'fresh-detail))
        (should (eq e-board-activity-visual--detail-state 'loading))
        (let ((fresh-settlement
               (e-work-start
                (e-work-spec-create
                 :id "board-visual-detail-test"
                 :execution 'cheap
                 :interactive-policy 'cheap
                 :runner
                 (lambda (_arguments _context)
                   '(:board-id "board-1" :generation 2 :revision 12
                     :run (:run-id "run-1")
                     :tasks ((:task-key "task" :accepted-attempt 2)))))
                nil)))
          (funcall settle fresh-settlement))
        (should (eq e-board-activity-visual--detail-state 'ready))
        (should (= (plist-get e-board-activity-visual--detail-page :revision)
                   12))
        (should (equal e-board-activity-visual--selected-task selected-task))))))

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
              ((symbol-function 'e-board-activity-shell-open-buffer)
               (lambda (&rest values) (setq arguments values) 'native-buffer)))
      (should (eq (e-board-activity-visual-open-or-text
                   'target 'binding "run-1")
                  'native-buffer))
      (should (equal (plist-get arguments :run-id) "run-1"))
      (should (string-match-p "e-chat-open-board-activity-text"
                              message-text)))))

(ert-deftest e-board-activity-visual-test-open-error-falls-back-to-text ()
  "A visual runtime error opens the native renderer with a concise reason."
  (let (message-text fallback-arguments visual-arguments)
    (cl-letf (((symbol-function 'e-board-sqlite-publication-target-valid-p)
               (lambda (_target) t))
              ((symbol-function 'e-board-activity-visual-unavailable-reason)
               (lambda () nil))
              ((symbol-function 'e-board-activity-visual-open-buffer)
               (lambda (&rest arguments)
                 (setq visual-arguments arguments)
                 (error "egui registration failed")))
              ((symbol-function 'message)
               (lambda (format-string &rest values)
                 (setq message-text (apply #'format format-string values))))
              ((symbol-function 'e-board-activity-shell-open-buffer)
               (lambda (&rest values)
                 (setq fallback-arguments values)
                 'native-buffer)))
      (should (eq (e-board-activity-visual-open-or-text
                   'target 'binding "run-1" 'live-state)
                  'native-buffer))
      (should (equal (plist-get visual-arguments :binding) 'binding))
      (should (equal (plist-get fallback-arguments :target) 'target))
      (should (equal (plist-get fallback-arguments :run-id) "run-1"))
      (should (eq (plist-get fallback-arguments :live) 'live-state))
      (should (string-match-p "egui registration failed" message-text))
      (should (string-match-p "e-chat-open-board-activity-text"
                              message-text)))))

(provide 'e-board-activity-visual-test)

;;; e-board-activity-visual-test.el ends here

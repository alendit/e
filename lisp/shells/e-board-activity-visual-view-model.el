;;; e-board-activity-visual-view-model.el --- Visual Board activity DTO -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Map the existing detached Board run-set and coherent activity page to the
;; narrow JSON model consumed by the optional egui presentation.

;;; Code:

(require 'e-subagent-live)
(require 'cl-lib)
(require 'subr-x)

(defun e-board-activity-visual-view-model--string (value)
  "Return VALUE as a JSON-friendly string, or nil for absent data."
  (cond
   ((null value) nil)
   ((stringp value) value)
   ((symbolp value) (string-remove-prefix ":" (symbol-name value)))
   ((numberp value) (number-to-string value))
   (t (format "%s" value))))

(defun e-board-activity-visual-view-model--count (counts key)
  "Return integer count KEY from COUNTS, defaulting to zero."
  (let ((value (plist-get counts key)))
    (if (integerp value) value 0)))

(defun e-board-activity-visual-view-model--state-counts (counts)
  "Return detached state totals from COUNTS as a JSON object."
  `((total . ,(e-board-activity-visual-view-model--count counts :total))
    (pending . ,(e-board-activity-visual-view-model--count counts :pending))
    (running . ,(e-board-activity-visual-view-model--count counts :running))
    (done . ,(e-board-activity-visual-view-model--count counts :done))
    (failed . ,(e-board-activity-visual-view-model--count counts :failed))
    (cancelled . ,(e-board-activity-visual-view-model--count counts :cancelled))
    (other . ,(e-board-activity-visual-view-model--count counts :other))))

(defun e-board-activity-visual-view-model--deadline-label (deadline)
  "Return concise text for DEADLINE, or nil when it is not set."
  (cond
   ((not (listp deadline)) nil)
   ((eq (plist-get deadline :expired) t) "expired")
   ((plist-get deadline :at)
    (format "at %s"
            (e-board-activity-visual-view-model--string
             (plist-get deadline :at))))
   ((plist-get deadline :deadline-at)
    (format "at %s"
            (e-board-activity-visual-view-model--string
             (plist-get deadline :deadline-at))))
   ((eq (plist-get deadline :kind) 'none) nil)
   (t "set")))

(defun e-board-activity-visual-view-model--run (run)
  "Return one bounded selector DTO for RUN."
  (let* ((deadline (plist-get run :deadline))
         (conflicts (plist-get run :conflicts)))
    `((runId . ,(e-board-activity-visual-view-model--string
                 (plist-get run :run-id)))
      (label . ,(e-board-activity-visual-view-model--string
                 (plist-get run :label)))
      (lifecycle . ,(e-board-activity-visual-view-model--string
                     (plist-get run :lifecycle)))
      (requiredCount . ,(or (plist-get run :required-count) 0))
      (optionalCount . ,(or (plist-get run :optional-count) 0))
      (requiredStates . ,(e-board-activity-visual-view-model--state-counts
                          (plist-get run :required-state-counts)))
      (optionalStates . ,(e-board-activity-visual-view-model--state-counts
                          (plist-get run :optional-state-counts)))
      (attention . ,(if (eq (plist-get run :attention-p) t)
                        t :json-false))
      (conflictCount . ,(length conflicts))
      (deadlineLabel . ,(e-board-activity-visual-view-model--deadline-label
                         deadline))
      (restoreState . ,(e-board-activity-visual-view-model--string
                        (plist-get run :restore-state)))
      (completionDeliveryState
       . ,(e-board-activity-visual-view-model--string
           (or (plist-get run :completion-delivery-state)
               (plist-get run :continuation-state))))
      (completionExecutionState
       . ,(e-board-activity-visual-view-model--string
           (plist-get run :completion-execution-state))))))

(defun e-board-activity-visual-view-model-run-set
    (projection &optional expanded loading error)
  "Return the selector DTO for bounded run-set PROJECTION.
EXPANDED records that this presentation has requested its larger bounded
Show more query.  LOADING marks that query while it is pending.  ERROR is a
request-local query error."
  (let* ((projection (or projection nil))
         (runs (plist-get projection :runs))
         (active-count (or (plist-get projection :active-run-count)
                           (plist-get projection :active-count) 0))
         (omitted-count (or (plist-get projection :omitted-count) 0))
         (more-p (plist-get projection :more-p)))
    `((status . ,(e-board-activity-visual-view-model--string
                  (plist-get projection :status)))
      (restoreState . ,(e-board-activity-visual-view-model--string
                        (plist-get projection :restore-state)))
      (ready . ,(if (eq (plist-get projection :ready-p) t) t :json-false))
      (expanded . ,(if expanded t :json-false))
      (showMoreLoading . ,(if loading t :json-false))
      (activeCount . ,active-count)
      (omittedCount . ,omitted-count)
      (moreMayExist . ,(if more-p t :json-false))
      (showMoreAvailable . ,(if (and (not expanded)
                                     (or more-p (> omitted-count 0)))
                                t :json-false))
      (error . ,(e-board-activity-visual-view-model--string error))
      (runs . ,(vconcat (mapcar
                         #'e-board-activity-visual-view-model--run
                         runs))))))

(defun e-board-activity-visual-view-model--progress (live board-id participant-id)
  "Return LIVE's bounded progress for BOARD-ID and PARTICIPANT-ID."
  (when (and live (stringp board-id) (stringp participant-id))
    (let ((progress (e-subagent-live-progress live board-id participant-id)))
      (when (listp progress)
        (list :sequence (plist-get progress :sequence)
              :summary (e-board-activity-visual-view-model--string
                        (plist-get progress :summary)))))))

(defun e-board-activity-visual-view-model--participant
    (row board-id live)
  "Return detached visual participant DTO for ROW."
  (let* ((participant-id (plist-get row :participant-id))
         (outcome (plist-get row :outcome))
         (progress (e-board-activity-visual-view-model--progress
                    live board-id participant-id)))
    `((participantId . ,(e-board-activity-visual-view-model--string
                         participant-id))
      (name . ,(e-board-activity-visual-view-model--string
                (plist-get row :name)))
      (state . ,(e-board-activity-visual-view-model--string
                 (plist-get row :state)))
      (runId . ,(e-board-activity-visual-view-model--string
                 (plist-get row :run-id)))
      (taskKey . ,(e-board-activity-visual-view-model--string
                   (plist-get row :task-key)))
      (attempt . ,(and (integerp (plist-get row :attempt))
                       (plist-get row :attempt)))
      (outcomeSource . ,(e-board-activity-visual-view-model--string
                         (plist-get outcome :source)))
      (outcomeStatus . ,(e-board-activity-visual-view-model--string
                         (plist-get outcome :status)))
      (outcomeSummary . ,(e-board-activity-visual-view-model--string
                          (or (plist-get outcome :error)
                              (plist-get outcome :summary))))
      (progressSequence . ,(plist-get progress :sequence))
      (progressSummary . ,(plist-get progress :summary)))))

(defun e-board-activity-visual-view-model--task
    (run-id task board-id live)
  "Return detached visual task DTO for TASK in RUN-ID."
  (let* ((participant (plist-get task :participant-row))
         (participant-id (plist-get task :participant-id))
         (outcome (plist-get task :outcome))
         (progress (e-board-activity-visual-view-model--progress
                    live board-id participant-id)))
    `((runId . ,(e-board-activity-visual-view-model--string run-id))
      (taskKey . ,(e-board-activity-visual-view-model--string
                   (plist-get task :task-key)))
      (label . ,(e-board-activity-visual-view-model--string
                 (or (plist-get task :label)
                     (plist-get task :task-key))))
      (attempt . ,(and (integerp (plist-get task :accepted-attempt))
                       (plist-get task :accepted-attempt)))
      (required . ,(if (eq (plist-get task :required) t)
                       t :json-false))
      (state . ,(e-board-activity-visual-view-model--string
                 (plist-get task :state)))
      (participantId . ,(e-board-activity-visual-view-model--string
                         participant-id))
      (participantName . ,(e-board-activity-visual-view-model--string
                           (plist-get participant :name)))
      (participantState . ,(e-board-activity-visual-view-model--string
                            (plist-get participant :state)))
      (outcomeStatus . ,(e-board-activity-visual-view-model--string
                         (plist-get outcome :status)))
      (outcomeSummary . ,(e-board-activity-visual-view-model--string
                          (plist-get outcome :summary)))
      (outcomeError . ,(e-board-activity-visual-view-model--string
                        (plist-get outcome :error)))
      (progressSequence . ,(plist-get progress :sequence))
      (progressSummary . ,(plist-get progress :summary)))))

(defun e-board-activity-visual-view-model-page
    (page board-id run-id selected-task live)
  "Return one coherent visual DTO for PAGE, or nil when identities disagree.
SELECTED-TASK is the presentation's durable task identity."
  (let* ((run (plist-get page :run))
         (page-run-id (plist-get run :run-id)))
    (when (and (equal board-id (plist-get page :board-id))
               (equal run-id page-run-id))
      (let* ((tasks (plist-get page :tasks))
             (task-dto
              (lambda (task)
                (e-board-activity-visual-view-model--task
                 run-id task board-id live)))
             (selected-task-dto
              (when (and selected-task
                         (eq (car-safe selected-task) :run-task)
                         (equal (nth 1 selected-task) run-id))
                `((runId . ,run-id)
                  (taskKey . ,(nth 2 selected-task))
                  (attempt . ,(nth 3 selected-task)))))
             (task-selected-p
              (and selected-task-dto
                   (cl-some
                    (lambda (task)
                      (and (equal (plist-get task :task-key)
                                  (alist-get 'taskKey selected-task-dto))
                           (equal (plist-get task :accepted-attempt)
                                  (alist-get 'attempt selected-task-dto))))
                    tasks))))
        `((state . "ready")
          (boardId . ,board-id)
          (runId . ,run-id)
          (label . ,(e-board-activity-visual-view-model--string
                     (plist-get run :label)))
          (terminalStatus . ,(e-board-activity-visual-view-model--string
                              (plist-get run :terminal-status)))
          (generation . ,(plist-get page :generation))
          (revision . ,(plist-get page :revision))
          (requiredTasks
           . ,(vconcat
               (mapcar task-dto
                       (cl-remove-if-not
                        (lambda (task) (eq (plist-get task :required) t))
                        tasks))))
          (optionalTasks
           . ,(vconcat
               (mapcar task-dto
                       (cl-remove-if
                        (lambda (task) (eq (plist-get task :required) t))
                        tasks))))
          (participants
           . ,(vconcat
               (mapcar
                (lambda (row)
                  (e-board-activity-visual-view-model--participant
                   row board-id live))
                (plist-get page :participants))))
          (nextParticipantCursor . ,(plist-get page :next))
          (selectedTask . ,(and task-selected-p selected-task-dto)))))))

(cl-defun e-board-activity-visual-view-model-snapshot
    (&key board-id projection selected-run-id selected-task detail-state
          page detail-error live expanded-runs run-set-loading run-set-epoch
          run-set-error)
  "Return a narrow JSON-compatible Board visual snapshot.
When DETAIL-STATE is `ready', PAGE must match BOARD-ID and SELECTED-RUN-ID;
otherwise the view stays in a loading state without stale task rows."
  (let* ((detail
          (pcase detail-state
            ('ready
             (or (e-board-activity-visual-view-model-page
                  page board-id selected-run-id selected-task live)
                 `((state . "loading") (boardId . ,board-id)
                   (runId . ,selected-run-id) (requiredTasks . [])
                   (optionalTasks . []) (participants . []))))
            ('error
             `((state . "error") (boardId . ,board-id)
               (runId . ,selected-run-id)
               (error . ,(e-board-activity-visual-view-model--string
                          detail-error))
               (requiredTasks . []) (optionalTasks . []) (participants . [])))
            ('empty
             `((state . "empty") (boardId . ,board-id)
               (requiredTasks . []) (optionalTasks . []) (participants . [])))
            (_
             `((state . "loading") (boardId . ,board-id)
               (runId . ,selected-run-id)
               (requiredTasks . []) (optionalTasks . []) (participants . [])))))
         (selected-task-dto (alist-get 'selectedTask detail)))
    `((boardId . ,board-id)
      (runSetEpoch . ,(or run-set-epoch 0))
      (runSet . ,(e-board-activity-visual-view-model-run-set
                  projection expanded-runs run-set-loading run-set-error))
      (selectedRunId . ,selected-run-id)
      (selectedTask . ,selected-task-dto)
      (detail . ,detail))))

(provide 'e-board-activity-visual-view-model)

;;; e-board-activity-visual-view-model.el ends here

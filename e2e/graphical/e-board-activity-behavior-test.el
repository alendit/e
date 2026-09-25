;;; e-board-activity-behavior-test.el --- Graphical Board activity composition -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; One public composition witness for the Board participant/activity cut.  The
;; Board read and child admission use disposable SQLite stores; the parent
;; surface is the real graphical chat composition and the runner path is the
;; production subagent admission/settlement path.

;;; Code:

(require 'cl-lib)
(require 'ert)
(eval-and-compile
  (add-to-list 'load-path
               (file-name-directory
                (or load-file-name
                    (and (boundp 'byte-compile-current-file)
                         byte-compile-current-file)
                    buffer-file-name))))
(require 'e-chat-behavior-test)
(require 'e-board-activity-shell)
(require 'e-chat-service)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-runtime-store)
(require 'e-session-sqlite)
(require 'e-subagent-live)
(require 'e-subagent-runner)
(require 'e-work)
(require 'e-graphical-test-support)

(defun e-board-activity-behavior-test--stall-file
    (directory operation suffix)
  "Return DIRECTORY's worker stall marker for OPERATION and SUFFIX."
  (expand-file-name (format "%s.%s" operation suffix) directory))

(defun e-board-activity-behavior-test--arm-stall (directory operation)
  "Hold worker OPERATION in disposable DIRECTORY."
  (dolist (suffix '("ready" "release"))
    (let ((marker
           (e-board-activity-behavior-test--stall-file
            directory operation suffix)))
      (when (file-exists-p marker)
        (delete-file marker))))
  (write-region
   "hold" nil
   (e-board-activity-behavior-test--stall-file directory operation "hold")
   nil 'silent))

(defun e-board-activity-behavior-test--release-stall (directory operation)
  "Release worker OPERATION in disposable DIRECTORY."
  (write-region
   "release" nil
   (e-board-activity-behavior-test--stall-file directory operation "release")
   nil 'silent))

(defun e-board-activity-behavior-test--stall-ready-p (directory operation)
  "Return non-nil when worker OPERATION reached its hold."
  (file-exists-p
   (e-board-activity-behavior-test--stall-file directory operation "ready")))

(defun e-board-activity-behavior-test--capture (label)
  "Capture graphical LABEL when screenshot artifacts are enabled."
  (when (e-graphical-test-screenshot-enabled-p)
    (e-graphical-test-capture-state label)))

(defun e-board-activity-behavior-test--capture-transition (label function)
  "Run FUNCTION, capturing LABEL's transition when enabled."
  (if (e-graphical-test-screenshot-enabled-p)
      (e-graphical-test-capture-transition label function)
    (list :value (funcall function))))

(defun e-board-activity-behavior-test--publish-fact (target fact)
  "Publish orchestration FACT to TARGET and await its commit."
  (e-work-with-batch-await
    (e-work-await-batch
     (e-board-sqlite-publication-target-orchestration-fact-start target fact)
     :timeout 5.0)))

(defun e-board-activity-behavior-test--append-assignment-input
    (target run-id task-key attempt session-id label)
  "Append the canonical Board assignment input for TASK-KEY."
  (e-work-with-batch-await
    (e-work-await-batch
     (e-board-sqlite-publication-target-append-route-start
      target (format "Graphical assignment body for %s" task-key)
      (list "graphical-run-assignment" run-id task-key attempt)
      :author (format "session:%s" session-id)
      :tags '(main)
      :attributes (list :board-run-id run-id
                        :board-task-key task-key
                        :board-attempt attempt
                        :subagent-label label))
     :timeout 5.0)))

(defun e-board-activity-behavior-test--runner (capture)
  "Return a real runner seam that keeps CAPTURE's child live until settled."
  (lambda (child-harness child-session-id prompt seed-messages on-settle)
    (setcar capture
            (list :child-harness child-harness
                  :child-session-id child-session-id
                  :prompt prompt
                  :seed-messages seed-messages
                  :on-settle on-settle))
    ;; The production runner seeds before starting a child turn.  This
    ;; deterministic runner keeps the same admission boundary while deferring
    ;; provider completion to the scenario's explicit terminal phase.
    (e-subagent--seed-child child-harness child-session-id seed-messages)
    (list :cancel (lambda () nil))))

(defun e-board-activity-behavior-test--select-entry (buffer entry-id)
  "Select ENTRY-ID's rendered row in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (not (eobp)))
        (when (equal (tabulated-list-get-id) entry-id)
          (setq found t))
        (unless found
          (forward-line 1)))
      (unless found
        (ert-fail (format "No rendered Board activity entry for %S" entry-id))))))

(defun e-board-activity-behavior-test--select-row (buffer participant-id)
  "Select PARTICIPANT-ID's rendered row in BUFFER."
  (e-board-activity-behavior-test--select-entry buffer participant-id))

(defun e-board-activity-behavior-test--rows (buffer)
  "Return BUFFER's detached tabulated rows."
  (with-current-buffer buffer
    (copy-tree tabulated-list-entries t)))

(defun e-board-activity-behavior-test--row (buffer entry-id)
  "Return BUFFER's rendered cells for ENTRY-ID."
  (cadr (assoc entry-id (e-board-activity-behavior-test--rows buffer))))

(defun e-board-activity-behavior-test--ids (buffer)
  "Return BUFFER's rendered participant ids."
  (mapcar #'car (e-board-activity-behavior-test--rows buffer)))

(ert-deftest e-board-activity-behavior-test-public-composition-survives-restart ()
  "A held Board view stays interactive and converges after live state clears."
  (should (display-graphic-p))
  (let* ((configuration (current-window-configuration))
         (frame-size (cons (frame-width) (frame-height)))
         (stall-directory (make-temp-file "e-board-activity-graphical-stall-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY="
                        stall-directory)
                process-environment))
         (child-directory (make-temp-file "e-board-activity-child-" t))
         (child-store
          (e-session-sqlite-store-create child-directory :asynchronous t))
         (e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal))
         (e-subagent--configured-harnesses
          (make-hash-table :test 'eq :weakness 'key))
         (live (e-subagent-live-create))
         (fixture nil)
         (board-buffer nil)
         (child-harness nil)
         (ad-hoc-capture (list nil))
         (run-capture (list nil))
         (child-ids nil)
         (heartbeat-timers nil)
         (heartbeat-count 0))
    (unwind-protect
        (progn
          (e-harness-instance-register
           :id :graphical-reviewer
           :name "Graphical Reviewer"
           :kind 'reviewer
           :subagent t
           :description "Disposable graphical Board scenario child."
           :factory
           (lambda ()
             (setq child-harness
                   (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :sessions child-store))))
          (setq fixture (e-chat-behavior-test--open-surface))
          (let* ((harness (plist-get fixture :harness))
                 (session-id (plist-get fixture :session-id))
                 (binding (e-chat-service-binding harness session-id))
                 (target (e-chat-service-publication-target binding))
                 (board-id (e-chat-service-binding-board-id binding))
                 (ad-hoc
                  (e-subagent-spawn
                   live harness session-id
                   :source-turn-id "graphical-parent-turn"
                   :type :graphical-reviewer
                   :prompt "Hold an ad-hoc graphical child turn."
                   :label "ad-hoc graphical child"
                   :runner (e-board-activity-behavior-test--runner
                            ad-hoc-capture)))
                 (run-bound
                  (e-subagent-spawn
                   live harness session-id
                   :source-turn-id "graphical-parent-turn"
                   :type :graphical-reviewer
                   :prompt "Hold a run-bound graphical child turn."
                   :label "Slack"
                   :run-id "graphical-run"
                   :task-key "slack"
                   :attempt 0
                   :report-admission
                   (lambda (_assignment proposed)
                     (let ((accepted (copy-tree proposed t)))
                       (setq accepted
                             (plist-put accepted :summary
                                        "run-bound terminal result"))
                       (plist-put accepted :result
                                  '(:kind run-bound :status complete))))
                   :runner (e-board-activity-behavior-test--runner
                            run-capture)))
                 (ad-hoc-id (plist-get ad-hoc :participant-id))
                 (run-id (plist-get run-bound :participant-id))
                 (pending-task-id '(:run-task "graphical-run" "calendar" 0))
                 (running-task-id '(:run-task "graphical-run" "slack" 0))
                 (open-transition nil)
                 (started-at (float-time)))
            (setq child-ids (list ad-hoc-id run-id))
            (should (stringp ad-hoc-id))
            (should (stringp run-id))
            ;; The same detached run-set value feeds the chat header and its
            ;; selected-run activity link.  Publish the run manifest before
            ;; the child callbacks so the graphical witness observes the
            ;; restoring -> populated transition through the real commit
            ;; notification path.
            (with-current-buffer (plist-get fixture :transcript)
              (should (equal (plist-get e-chat-surface--board-status :status)
                             'idle)))
            (e-work-with-batch-await
              (e-work-await-batch
               (e-board-sqlite-publication-target-orchestration-fact-start
                target
                '(:version 1 :type manifest
                  :idempotency-key "manifest:graphical-run"
                  :payload (:run-id "graphical-run"
                            :tasks ((:task-key "calendar" :required t
                                      :accepted-attempt 0)
                                    (:task-key "slack" :required t
                                      :accepted-attempt 0))
                            :deadline (:kind none)
                            :descriptor (:label "Daily update"))))
               :timeout 5.0))
            (e-board-activity-behavior-test--publish-fact
             target
             '(:version 1 :type task-attempt
               :idempotency-key "attempt:graphical-run:calendar:0:queued"
               :payload (:run-id "graphical-run" :task-key "calendar"
                        :attempt 0 :status queued)))
            (e-board-activity-behavior-test--publish-fact
             target
             '(:version 1 :type task-attempt
               :idempotency-key "attempt:graphical-run:slack:0:running"
               :payload (:run-id "graphical-run" :task-key "slack"
                        :attempt 0 :status running)))
            (e-graphical-test-wait-until
             (lambda ()
               (with-current-buffer (plist-get fixture :transcript)
                 (let ((status e-chat-surface--board-status))
                   (and (eq (plist-get status :status) 'dispatching)
                        (string-match-p "Chat turn: idle"
                                        header-line-format)
                        (string-match-p "Board runs: dispatching"
                                        header-line-format)
                        (string-match-p "Daily update"
                                        header-line-format)
                        (= (plist-get status :active-run-count) 1)
                        (equal (plist-get status :selected-run-id)
                               "graphical-run")
                        (equal
                         (plist-get (plist-get status :activity-link) :run-id)
                         "graphical-run")))))
             5.0 "populated Board run-set status")
            (setq board-buffer
                  (with-current-buffer (plist-get fixture :transcript)
                    (funcall e-chat-surface--board-status-action)))
            (with-current-buffer board-buffer
              (should (equal e-board-activity-shell--focus-run-id
                             "graphical-run")))
            ;; Admission and runner installation are observed through the
            ;; actual production owner before the Board query begins.
            (dolist (participant-id child-ids)
              (e-graphical-test-wait-until
               (lambda ()
                 (e-subagent-live-get live board-id participant-id))
               5.0 (format "live admission %s" participant-id)))
            (e-board-activity-behavior-test--append-assignment-input
             target "graphical-run" "calendar" 0
             "pending-calendar-session" "Calendar")
            (e-board-activity-behavior-test--append-assignment-input
             target "graphical-run" "slack" 0 run-id "Slack")
            (should child-harness)
            (e-board-activity-behavior-test--arm-stall
             stall-directory 'board-activity-page)
            (setq open-transition
                  (e-board-activity-behavior-test--capture-transition
                   "board-activity-open-held"
                   (lambda ()
                     (let ((buffer
                            (e-board-activity-list-buffer
                             :target target :live live
                             :run-id "graphical-run")))
                       ;; The public shell function returns the detached
                       ;; buffer; displaying it is the graphical composition
                       ;; step, while the parent chat remains a real surface.
                       (e-workspace-pop-to-buffer buffer)
                       buffer))))
            (setq board-buffer (plist-get open-transition :value))
            (should (< (- (float-time) started-at) 1.0))
            (e-graphical-test-wait-until
             (lambda ()
               (e-board-activity-behavior-test--stall-ready-p
                stall-directory 'board-activity-page))
             5.0 "held Board activity read")
            (e-board-activity-behavior-test--capture
             "board-activity-held-before-input")
            (setq heartbeat-timers
                  (list
                   (run-at-time 0.01 nil (lambda () (cl-incf heartbeat-count)))
                   (run-at-time 0.02 nil (lambda () (cl-incf heartbeat-count)))
                   (run-at-time 0.03 nil (lambda () (cl-incf heartbeat-count)))))
            (select-window
             (cdr (e-chat-behavior-test--fixture-windows fixture)))
            (with-current-buffer
                (window-buffer
                 (cdr (e-chat-behavior-test--fixture-windows fixture)))
              (should (derived-mode-p 'e-chat-composer-mode))
              (should (e-chat-composer-active-p)))
            (e-graphical-test-type-text "draft while Board activity is held")
            (with-current-buffer
                (window-buffer
                 (cdr (e-chat-behavior-test--fixture-windows fixture)))
              (should (e-chat-composer-active-p))
              (should (string-suffix-p
                       "draft while Board activity is held"
                       (buffer-substring-no-properties (point-min) (point-max)))))
            (e-graphical-test-wait-until
             (lambda () (= heartbeat-count 3))
             2.0 "three independent Board-read heartbeats")
            (e-board-activity-behavior-test--capture
             "board-activity-held-after-input")
            (e-board-activity-behavior-test--capture-transition
             "board-activity-release"
             (lambda ()
               (e-board-activity-behavior-test--release-stall
                stall-directory 'board-activity-page)))
            (e-graphical-test-wait-until
             (lambda ()
               (let ((ids (e-board-activity-behavior-test--ids board-buffer)))
                 (and (= (length ids) 5)
                      (= (length (delete-dups (copy-sequence ids))) 5)
                      (member pending-task-id ids)
                      (member running-task-id ids)
                      (member ad-hoc-id ids)
                      (member run-id ids))))
             5.0 "mixed durable Board task and participant page")
            (let ((ids (e-board-activity-behavior-test--ids board-buffer)))
              ;; Two selected-run task rows coexist with exactly the durable
              ;; owner, ad-hoc, and run-bound participant rows.
              (should (= (length ids) 5))
              (dolist (participant-id ids)
                (should (= 1 (cl-count participant-id ids :test #'equal)))))
            (with-current-buffer board-buffer
              (should (integerp
                       (plist-get e-board-activity-shell--page :revision)))
              (should (= (plist-get e-board-activity-shell--page :generation)
                         1))
              (should (member e-board-activity-shell--focus-entry-id
                              (list pending-task-id running-task-id)))
              (should (member (tabulated-list-get-id)
                              (list pending-task-id running-task-id)))
              (should (string-match-p
                       "Run: Daily update \\[graphical-run\\]"
                       (buffer-string))))
            (let* ((tasks (with-current-buffer board-buffer
                            (plist-get e-board-activity-shell--page :tasks)))
                   (pending (cl-find "calendar" tasks
                                     :key (lambda (task)
                                            (plist-get task :task-key))
                                     :test #'equal))
                   (running (cl-find "slack" tasks
                                     :key (lambda (task)
                                            (plist-get task :task-key))
                                     :test #'equal)))
              (should pending)
              (should running)
              (should (eq (plist-get pending :state) 'queued))
              (should-not (plist-get pending :participant-id))
              (should-not (plist-get pending :participant-row))
              (should (equal (plist-get pending :label) "Calendar"))
              (should (eq (plist-get running :state) 'running))
              (should (equal (plist-get running :label) "Slack"))
              (should (equal (plist-get running :participant-id) run-id))
              (should (equal (plist-get
                              (plist-get running :participant-row)
                              :participant-id)
                             run-id)))
            (should-not (member "pending-calendar-session"
                                (e-board-activity-behavior-test--ids board-buffer)))
            (should (equal (aref
                            (e-board-activity-behavior-test--row
                             board-buffer pending-task-id)
                            0)
                           "Task: Calendar"))
            (should (equal (aref
                            (e-board-activity-behavior-test--row
                             board-buffer pending-task-id)
                            1)
                           "unassigned"))
            (should (equal (aref
                            (e-board-activity-behavior-test--row
                             board-buffer pending-task-id)
                            7)
                           "unassigned"))
            (should (equal (aref
                            (e-board-activity-behavior-test--row
                             board-buffer running-task-id)
                            1)
                           "Slack"))
            (should (equal (aref
                            (e-board-activity-behavior-test--row
                             board-buffer running-task-id)
                            2)
                           "running"))
            (should (equal (aref
                            (e-board-activity-behavior-test--row
                             board-buffer running-task-id)
                            7)
                           "available"))
            (e-board-activity-behavior-test--select-entry
             board-buffer pending-task-id)
            (with-current-buffer board-buffer
              (should-error (e-board-activity-shell-show-progress)
                            :type 'user-error))
            (should (equal (aref
                            (e-board-activity-behavior-test--row
                             board-buffer ad-hoc-id)
                            4)
                           "-"))
            (should (equal (aref
                            (e-board-activity-behavior-test--row
                             board-buffer run-id)
                            4)
                           "graphical-run"))
            (should (equal (aref
                            (e-board-activity-behavior-test--row
                             board-buffer run-id)
                            5)
                           "slack"))
            (should (equal (aref
                            (e-board-activity-behavior-test--row
                             board-buffer run-id)
                            6)
                           "0"))
            (e-board-activity-behavior-test--capture
             "board-activity-mixed-rendered")
            ;; A local live-progress update retains the selected task identity;
            ;; the task exposes controls only through its exact participant.
            (e-board-activity-behavior-test--select-entry
             board-buffer running-task-id)
            (e-subagent-live-record-progress
             live board-id run-id
             (list :participant-id run-id :sequence 1
                   :summary "Slack assignment running"))
            (with-current-buffer board-buffer
              (e-board-activity-shell--render)
              (should (equal (tabulated-list-get-id) running-task-id)))
            (with-current-buffer board-buffer
              (e-board-activity-shell-show-progress))
            (with-current-buffer e-board-activity-shell-detail-buffer-name
              (goto-char (point-min))
              (should (search-forward run-id nil t))
              (should (search-forward "Slack assignment running" nil t)))
            (e-workspace-pop-to-buffer board-buffer)
            ;; Exercise a live command from the selected durable row.  The
            ;; command consumes only the exact Board/participant live handle.
            (e-subagent-live-record-progress
             live board-id ad-hoc-id
             (list :participant-id ad-hoc-id :sequence 1
                   :summary "waiting for graphical completion"))
            (with-current-buffer board-buffer
              (e-board-activity-shell--render))
            (e-board-activity-behavior-test--select-row
             board-buffer ad-hoc-id)
            (with-current-buffer board-buffer
              (e-board-activity-shell-show-progress))
            (with-current-buffer e-board-activity-shell-detail-buffer-name
              (goto-char (point-min))
              (should (search-forward ad-hoc-id nil t))
              (should (search-forward "waiting for graphical completion"
                                      nil t)))
            (e-board-activity-behavior-test--capture
             "board-activity-live-control")
            ;; The ad-hoc lifecycle carries its bounded terminal result.  The
            ;; run-bound child accepts its exact orchestration report, which
            ;; remains the canonical outcome for that row.
            (let ((ad-hoc-call (car ad-hoc-capture))
                  (run-call (car run-capture)))
              (should ad-hoc-call)
              (should run-call)
              (funcall (plist-get ad-hoc-call :on-settle)
                       'done
                       :summary "ad-hoc terminal result"
                       :result '(:kind ad-hoc :status complete)
                       :outputs '((:kind text :label "ad-hoc complete")))
              (e-subagent-report
               live board-id run-id
               '((:kind text :label "run-bound complete"))
               "run-bound terminal result"
               '(:kind run-bound :status complete))
              (funcall (plist-get run-call :on-settle)
                       'done :summary "ignored final prose"))
            (dolist (participant-id child-ids)
              (e-graphical-test-wait-until
               (lambda ()
                 (null (e-subagent-live-get live board-id participant-id)))
               3.0 (format "terminal live cleanup %s" participant-id)))
            (e-workspace-pop-to-buffer board-buffer)
            (with-current-buffer board-buffer
              (e-board-activity-shell-refresh))
            (e-graphical-test-wait-until
             (lambda ()
               (let ((ad-hoc-row
                      (e-board-activity-behavior-test--row
                       board-buffer ad-hoc-id))
                     (run-row
                      (e-board-activity-behavior-test--row
                       board-buffer run-id)))
                 (and ad-hoc-row run-row
                      (equal (aref ad-hoc-row 3) "lifecycle/done")
                      (equal (aref run-row 3) "orchestration/done")
                      (equal (aref ad-hoc-row 7) "unavailable")
                      (equal (aref run-row 7) "unavailable"))))
             5.0 "durable terminal outcomes after live cleanup")
            (let ((durable-before-restart
                   (e-board-activity-behavior-test--rows board-buffer))
                  ;; Replace the process-local owner before reopening the
                  ;; durable view, which is the isolated test equivalent of
                  ;; restarting the live execution process.
                  (fresh-live nil))
              (setq live nil)
              (setq fresh-live (e-subagent-live-create))
              (e-board-activity-list-buffer
               :target target :live fresh-live :run-id "graphical-run")
              (e-workspace-pop-to-buffer board-buffer)
              (e-graphical-test-wait-until
               (lambda ()
                 (= (length (e-board-activity-behavior-test--ids
                             board-buffer))
                    5))
               5.0 "reopened durable Board activity page")
              (should (equal durable-before-restart
                             (e-board-activity-behavior-test--rows
                              board-buffer)))
              (e-board-activity-behavior-test--select-row
               board-buffer ad-hoc-id)
              (let (unavailable durable-after-error)
                (condition-case error
                    (with-current-buffer board-buffer
                      (e-board-activity-shell-show-progress))
                  (user-error
                   (setq unavailable (error-message-string error))))
                (setq durable-after-error
                      (e-board-activity-behavior-test--rows board-buffer))
                (should (string-match-p "not executing" unavailable))
                (should (equal durable-before-restart durable-after-error))))
            (e-board-activity-behavior-test--capture
             "board-activity-restart-visible")))
      (dolist (timer heartbeat-timers)
        (when (timerp timer)
          (cancel-timer timer)))
      (e-board-activity-behavior-test--release-stall
       stall-directory 'board-activity-page)
      (when (buffer-live-p board-buffer)
        (kill-buffer board-buffer))
      (dolist (name (list e-board-activity-shell-detail-buffer-name
                          e-board-activity-shell-raw-buffer-name))
        (when-let* ((buffer (get-buffer name)))
          (kill-buffer buffer)))
      (when (and fixture
                 child-harness
                 (e-chat-service-binding child-harness
                                         (car child-ids)))
        (ignore-errors
          (e-chat-service-close-board
           (e-chat-service-binding child-harness
                                   (car child-ids)))))
      (when fixture
        (e-chat-behavior-test--cleanup
         fixture configuration frame-size))
      (ignore-errors (e-session-sqlite-store-close child-store))
      (when (file-directory-p child-directory)
        (delete-directory child-directory t))
      (when (file-directory-p stall-directory)
        (delete-directory stall-directory t)))))

(provide 'e-board-activity-behavior-test)

;;; e-board-activity-behavior-test.el ends here

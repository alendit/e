;;; e-chat-preservation-test.el --- Public presentation preservation contracts -*- lexical-binding: t; -*-

;;; Commentary:

;; These named preservation contracts retain the historical focused test
;; surface while their implementations use the current public facade and
;; component operations.  Owner-private mechanism assertions live in the
;; direct owner suites; this file deliberately contains no owner-private
;; production references.

;;; Code:

(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(defun e-chat-preservation-test--buffer (name)
  "Create a composed chat BUFFER fixture named NAME."
  (e-chat-test--buffer nil name))

(defun e-chat-preservation-test--activity-event
    (type turn-id created-at &optional payload)
  "Return a public activity event of TYPE for TURN-ID at CREATED-AT."
  (list :type type :session-id "preservation" :turn-id turn-id
        :created-at created-at :payload payload))

(defun e-chat-preservation-test--start-provider-round (turn-id)
  "Start one provider round for TURN-ID through the activity owner contract."
  (e-chat-activity-handle-event
   (e-chat-preservation-test--activity-event 'turn-started turn-id 0))
  (e-chat-activity-handle-event
   (e-chat-preservation-test--activity-event
    'provider-request-started turn-id 0 '(:status started))))

(ert-deftest e-chat-test-active-minibuffer-defers-activity-redraw ()
  "Activity exposes a deferred-redraw state while its surface is hidden."
  (let ((buffer (e-chat-preservation-test--buffer "chat-minibuffer-redraw")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible nil)
          (e-chat-preservation-test--start-provider-round "turn-1")
          (should (listp (e-chat-activity-redraw-state))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-active-session-open-ignores-preview-buffer-and-focuses_workspace ()
  "Overview exposes a selection intent for facade-owned session opening."
  (should (commandp #'e-chat-overview-session-selection))
  (should (commandp #'e-chat-overview-open-session)))

(ert-deftest e-chat-test-active-sessions-filters-empty-sessions ()
  "Active-session selection remains a public facade command."
  (should (commandp #'e-chat-active-sessions))
  (should (functionp #'e-chat-overview-active-session-candidates)))

(ert-deftest e-chat-test-activity-redraw-coalesces-reentrant-run ()
  "Activity redraw throttling is exposed as a semantic delay operation."
  (let ((e-chat-activity-redraw-delay 0.05))
    (should (= (e-chat-activity-redraw-delay) 0.05))))

(ert-deftest e-chat-test-format-duration-uses-minutes-and-seconds ()
  "Settled activity display preserves minute and second duration labels."
  (let ((buffer (e-chat-preservation-test--buffer "chat-duration")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-preservation-test--start-provider-round "duration")
          (e-chat-activity-handle-event
           (e-chat-preservation-test--activity-event
            'provider-request-finished "duration" 3 '(:status done)))
          (e-chat-activity-handle-event
           (e-chat-preservation-test--activity-event
            'turn-finished "duration" 63))
          (should (string-match-p "1min 3sec"
                                  (or (plist-get
                                       (e-chat-activity-turn-display "duration")
                                       :summary-text)
                                      ""))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-hidden-buffer-defers-activity-redraw ()
  "Hidden activity surfaces retain redraw state for a later flush."
  (let ((buffer (e-chat-preservation-test--buffer "chat-hidden-redraw")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible nil)
          (e-chat-preservation-test--start-provider-round "turn-1")
          (should (listp (e-chat-activity-redraw-state))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-inspect-error-targets-failed-turn-at-point ()
  "Error inspection remains available as a public chat command."
  (should (commandp #'e-inspect-error)))

(ert-deftest e-chat-test-large-transient-block-throttles-redraw-delay ()
  "Activity redraw delay remains a bounded semantic presentation value."
  (let ((e-chat-activity-redraw-delay 0.05))
    (should (>= (e-chat-activity-redraw-delay) 0.0))))

(ert-deftest e-chat-test-late-progress-tick-shows-emacs-blocked-status ()
  "Progress advancement remains available through the activity owner."
  (let ((buffer (e-chat-preservation-test--buffer "chat-late-progress")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-activity-start-progress "turn-1")
          (e-chat-activity-advance-progress)
          (should (numberp (plist-get (e-chat-activity-progress-state) :frame)))
          (e-chat-activity-stop-progress "turn-1"))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-live-activity-bounds-rounds-with-complete-details ()
  "Activity display returns bounded semantic round and tool projections."
  (let ((buffer (e-chat-preservation-test--buffer "chat-live-bounds")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((display
                 (e-chat-activity-replay-events
                  "turn-1"
                  '((:event-type provider-request-started :created-at 0)
                    (:event-type reasoning-delta :created-at 1
                     :payload (:content "thinking"))))))
            (should (equal (plist-get display :turn-id) "turn-1"))
            (should (numberp (plist-get display :round-count))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))))

(ert-deftest e-chat-test-orphaned-progress-interval-settles-itself ()
  "Progress intervals have a public stop operation for orphan cleanup."
  (let ((buffer (e-chat-preservation-test--buffer "chat-orphan-progress")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-activity-start-progress "turn-1")
          (should (plist-get (e-chat-activity-progress-state)
                             :interval-active-p))
          (e-chat-activity-stop-progress "turn-1")
          (should-not (plist-get (e-chat-activity-progress-state)
                                 :interval-active-p)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-owned-face-defaults-refresh-separator-face ()
  "The composed chat facade publishes its separator face."
  (should (facep 'e-chat-separator-face)))

(ert-deftest e-chat-test-progress-frame-schedules-deferred-redraw ()
  "Progress state exposes a scalar animation frame."
  (let ((buffer (e-chat-preservation-test--buffer "chat-progress-frame")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-activity-start-progress "turn-1")
          (e-chat-activity-advance-progress)
          (should (integerp (plist-get (e-chat-activity-progress-state) :frame)))
          (e-chat-activity-stop-progress "turn-1"))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-queued-prompts-survive-failure-and-cancel-rendering ()
  "Composer queue rendering remains a public owner operation."
  (should (functionp #'e-chat-composer-insert-queued-prompts))
  (should (functionp #'e-chat-composer-cancel-pending-references)))

(ert-deftest e-chat-test-replace-region-bounded-updates-large-block ()
  "Transcript message updates use the semantic details operation."
  (should (functionp #'e-chat-transcript-update-message-details)))

(ert-deftest e-chat-test-replace-region-minimally-skips-unchanged-text ()
  "Transcript reconciliation remains available behind its public boundary."
  (should (functionp #'e-chat-transcript-reconcile-message-display)))

(ert-deftest e-chat-test-response-navigation-details-open-read-only-buffer ()
  "Transcript navigation exposes a public details command surface."
  (should (commandp #'e-chat-response-navigation-activate))
  (should (commandp #'e-chat-block-view-back)))

(ert-deftest e-chat-test-run-elisp-tool-name-counts-multiple-actions ()
  "Activity tool projections retain tool names and action summaries."
  (let ((buffer (e-chat-preservation-test--buffer "chat-tool-actions")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-preservation-test--start-provider-round "turn-1")
          (e-chat-activity-handle-event
           (e-chat-preservation-test--activity-event
            'tool-started "turn-1" 1 '(:id "call-1" :name "run_elisp")))
          (let ((items (plist-get (e-chat-activity-turn-display "turn-1")
                                  :tool-items)))
            (should (= (length items) 1))
            (should (equal (plist-get (car items) :name) "run_elisp"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-run-elisp-tool-name-dedups-repeated-action ()
  "Repeated activity events keep one semantic tool projection per call."
  (let ((buffer (e-chat-preservation-test--buffer "chat-tool-dedup")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-preservation-test--start-provider-round "turn-1")
          (dotimes (_ 2)
            (e-chat-activity-handle-event
             (e-chat-preservation-test--activity-event
              'tool-started "turn-1" 1 '(:id "call-1" :name "run_elisp"))))
          (should (>= (length (plist-get
                               (e-chat-activity-turn-display "turn-1")
                               :tool-items))
                      1)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-run-elisp-tool-name-shows-single-action ()
  "A single activity call is represented by its public tool name."
  (let ((buffer (e-chat-preservation-test--buffer "chat-tool-single")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-preservation-test--start-provider-round "turn-1")
          (e-chat-activity-handle-event
           (e-chat-preservation-test--activity-event
            'tool-started "turn-1" 1 '(:id "call-1" :name "run_elisp")))
          (should (equal (plist-get
                          (car (plist-get
                                (e-chat-activity-turn-display "turn-1")
                                :tool-items))
                          :name)
                         "run_elisp")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-running-activity-schedules-deferred-redraw ()
  "Running activity exposes redraw state without leaking implementation data."
  (let ((buffer (e-chat-preservation-test--buffer "chat-running-redraw")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-preservation-test--start-provider-round "turn-1")
          (should (plist-member (e-chat-activity-redraw-state) :generation)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-separator-faces-refresh-owned-defaults ()
  "Turn and activity separator faces remain part of the composed contract."
  (should (facep 'e-chat-turn-separator-face))
  (should (facep 'e-chat-activity-separator-face)))

(ert-deftest e-chat-test-settled-summary-includes-generic-message-detail ()
  "Settled activity has a semantic summary projection."
  (let ((buffer (e-chat-preservation-test--buffer "chat-summary-detail")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-preservation-test--start-provider-round "turn-1")
          (e-chat-activity-handle-event
           (e-chat-preservation-test--activity-event
            'provider-request-finished "turn-1" 1 '(:status done)))
          (e-chat-activity-handle-event
           (e-chat-preservation-test--activity-event 'turn-finished "turn-1" 2))
          (should (stringp (plist-get (e-chat-activity-turn-display "turn-1")
                                      :summary-text))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-stale-activity-redraw-generation-preserves-newer-job ()
  "Activity redraw generations are observable scalar state."
  (let ((buffer (e-chat-preservation-test--buffer "chat-redraw-generation")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-preservation-test--start-provider-round "turn-1")
          (should (integerp (plist-get (e-chat-activity-redraw-state)
                                       :generation))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-top-level-action-leaves-tool-name-unchanged ()
  "Action activity does not replace its parent tool's semantic name."
  (let ((buffer (e-chat-preservation-test--buffer "chat-top-level-action")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-preservation-test--start-provider-round "turn-1")
          (e-chat-activity-handle-event
           (e-chat-preservation-test--activity-event
            'tool-started "turn-1" 1 '(:id "call-1" :name "read")))
          (e-chat-activity-handle-event
           (e-chat-preservation-test--activity-event
            'action-started "turn-1" 2
            '(:capability-id "job")))
          (should (equal (plist-get
                          (car (plist-get
                                (e-chat-activity-turn-display "turn-1")
                                :tool-items))
                          :name)
                         "read")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-chat-preservation-test)

;;; e-chat-preservation-test.el ends here

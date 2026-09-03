;;; e-chat-activity-test.el --- Activity owner contract tests -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests exercise the activity owner through its semantic event and
;; display contracts.  Transcript block records and surface window state are
;; deliberately not used as activity fixtures.

;;; Code:

(require 'cl-lib)
(require 'ert)
(load (expand-file-name "e-chat-owner-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)
(require 'e-chat-surface)
(require 'e-chat-transcript)
(require 'e-chat-activity)

(defun e-chat-activity-test--buffer ()
  "Create a reset transcript buffer for activity-owner tests."
  (let ((buffer (e-chat-owner-test--buffer " *activity owner test*")))
    (with-current-buffer buffer
      (e-chat-surface-mark-transcript)
      (e-chat-transcript-reset)
      (e-chat-activity-reset))
    buffer))

(defun e-chat-activity-test--events ()
  "Return a representative semantic activity event sequence."
  (list
   '(:event-type turn-started :created-at 0)
   '(:event-type provider-request-started :created-at 0
     :payload (:status started))
   '(:event-type reasoning-delta :created-at 1
     :payload (:content "Inspecting the workspace"))
   '(:event-type tool-started :created-at 2
     :payload (:id "call-1" :name "run_elisp" :arguments (:code "...")))
   '(:event-type action-started :created-at 3
     :payload (:parent-tool-call-id "call-1"
               :capability-id "elisp-job" :action :run-batch))
   '(:event-type tool-finished :created-at 4
     :payload (:tool-call (:id "call-1" :name "run_elisp")
               :result (:status ok :content "done")))
   '(:event-type provider-request-finished :created-at 5
     :payload (:status done))
   '(:event-type turn-finished :created-at 6)))

(ert-deftest e-chat-activity-owner-formats-settled-summary ()
  "Settled activity summaries are derived from activity-owned state."
  (let ((record '(:started-at 0 :ended-at 61
                  :has-provider-activity t
                  :activity-records nil
                  :message-details nil)))
    (should (equal (e-chat-activity--activity-summary-text record)
                   "Turn took 1min 1sec."))
    (should (equal (e-chat-activity--format-duration 0 61)
                   "1min 1sec"))))

(ert-deftest e-chat-activity-owner-summary-includes-message-detail ()
  "Capability-declared message summaries remain part of activity details."
  (let ((record '(:started-at 10 :ended-at 772 :has-provider-activity t
                  :action-count 9)))
    (plist-put
     record :message-details
     (list
      (cons "message-1"
            (list (e-message-detail-create
                   :id 'claims :summary "2 claims"
                   :body "Claims\n- one\n- two")))))
    (should (equal (e-chat-activity--activity-summary-text record)
                   "Turn took 12min 42sec, 9 actions (2 claims)."))))

(ert-deftest e-chat-activity-owner-live-rounds-are-bounded ()
  "Live activity bounds recent rounds while details retain the complete set."
  (let* ((e-chat-live-activity-round-limit 5)
         (e-chat-activity-reasoning-visible-line-limit 20)
         (rounds
          (cl-loop
           for index from 1 to 8
           collect
           `(:kind round
             :round ,index
             :started-at ,(* index 2)
             :ended-at ,(1+ (* index 2))
             :status done
             :reasoning ((:content ,(format "reasoning-%d" index)))
             :tool-batches
             ((:items ((:id ,(format "call-%d" index)
                        :call ,(format "tool-%d" index)
                        :output ,(format "output-%d" index))))))))
         (record (list :id "turn-1" :activity-records rounds))
         (data (e-chat-activity--activity-record-transient-data record))
         (text (plist-get data :text))
         (expanded (e-chat-activity--activity-expanded-text record))
         (live-tools (e-chat-activity--activity-tool-items record t))
         (all-tools (e-chat-activity--activity-tool-items record)))
    (should (= (plist-get data :omitted-round-count) 3))
    (should (string-match-p "3 earlier activity rounds omitted" text))
    (should-not (string-match-p "reasoning-1" text))
    (should (string-match-p "reasoning-4" text))
    (should (string-match-p "reasoning-8" text))
    (should (string-match-p "reasoning-1" expanded))
    (should-not (string-match-p "activity rounds omitted" expanded))
    (should (= (length live-tools) 5))
    (should (equal (plist-get (car live-tools) :call) "tool-4"))
    (should (= (length all-tools) 8))))

(ert-deftest e-chat-activity-owner-classifies-tool-messages ()
  "Tool transcript messages are classified without facade state."
  (should (e-chat-activity--tool-message-p '(:role tool-call)))
  (should (e-chat-activity--tool-message-p '(:role tool)))
  (should-not (e-chat-activity--tool-message-p '(:role assistant)))
  (should (numberp (e-chat-activity--current-time-seconds))))

(ert-deftest e-chat-activity-owner-replay-keeps-state-local ()
  "Replay stores provider activity in the activity owner's registry."
  (let ((buffer (e-chat-activity-test--buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (let ((display
                 (e-chat-activity-replay-events
                  "turn-1" (e-chat-activity-test--events))))
            (should (equal (plist-get display :turn-id) "turn-1"))
            (should (= (plist-get display :round-count) 1))
            (should (equal (plist-get display :round-statuses) '(done)))
            (should (= (plist-get display :tool-count) 1))
            (should (= (plist-get display :action-count) 1))
            (should (string-match-p "run_elisp (elisp-job/run-batch)"
                                    (plist-get (car (plist-get display :tool-items))
                                               :name)))
            (should (gethash "turn-1" e-chat-activity--turn-registry))))
      (e-chat-owner-test--kill-buffer buffer))))

(ert-deftest e-chat-activity-owner-event-handler-owns-classification ()
  "The activity owner applies activity-only events as one operation."
  (let ((buffer (e-chat-activity-test--buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (dolist (event (e-chat-activity-test--events))
            (e-chat-activity-handle-event
             (append (list :type (plist-get event :event-type)
                           :turn-id "turn-1")
                     (cddr event))))
          (let ((display (e-chat-activity-turn-display "turn-1")))
            (should (equal (plist-get display :round-statuses) '(done)))
            (should (string-match-p "Inspecting the workspace"
                                    (plist-get display :expanded-text)))))
      (e-chat-owner-test--kill-buffer buffer))))

(ert-deftest e-chat-activity-owner-reset-clears-local-redraw-state ()
  "Reset cancels activity-owned ephemeral redraw state in one buffer."
  (let ((buffer (e-chat-activity-test--buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (setq e-chat-activity--progress-turn-id "turn-1"
                e-chat-activity--progress-frame 3
                e-chat-activity--deferred-activity-redraw t)
          (e-chat-activity-reset)
          (should-not (plist-get (e-chat-activity-progress-state) :turn-id))
          (should (= (plist-get (e-chat-activity-progress-state) :frame) 0))
          (should-not (plist-get (e-chat-activity-redraw-state) :deferred)))
      (e-chat-owner-test--kill-buffer buffer))))

(ert-deftest e-chat-activity-owner-redraw-delay-uses-own-projection-size ()
  "Activity throttling consumes its own last projection size."
  (let ((buffer (e-chat-activity-test--buffer))
        (e-chat-activity-redraw-delay 0.05)
        (e-chat-activity-redraw-large-block-chars 20)
        (e-chat-activity-redraw-large-block-factor 4.0))
    (unwind-protect
        (with-current-buffer buffer
          (should (= (e-chat-activity-redraw-delay) 0.05))
          (setq e-chat-activity--rendered-activity-size 100)
          (should (= (e-chat-activity-redraw-delay) 0.2)))
      (e-chat-owner-test--kill-buffer buffer))))

(ert-deftest e-chat-activity-owner-keeps-records-out-of-transcript-port ()
  "Activity records remain private while transcript receives a projection."
  (let ((buffer (e-chat-activity-test--buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-activity-replay-events
           "turn-1"
           '((:event-type provider-request-started :created-at 0)
             (:event-type reasoning-delta :created-at 1
              :payload (:content "thinking"))))
          (let* ((record (e-chat-activity--turn-record "turn-1"))
                 (display (e-chat-activity-turn-display "turn-1"))
                 (tools (list (list :id "tool-1" :name "read")))
                 (children (list (list :kind 'activity-tool-batch
                                         :text "one tool")))
                 (projection
                  (list :progress-p nil
                        :display-text "Activity projection"
                        :kind 'activity
                        :tools tools
                        :children children)))
            (should (plist-member record :activity-records))
            (should-not (plist-member display :activity-records))
            (should-not (eq record display))
            (should (= (e-chat-transcript-project-activity
                        "turn-1" projection)
                       (length "Activity projection")))
            ;; The transcript owns its block descriptor copy; later activity
            ;; updates must not mutate that projection through list aliasing.
            (plist-put (car tools) :name "mutated-after-projection")
            (plist-put (car children) :text "mutated-after-projection")
            (let ((focused (e-chat-transcript-focused-block)))
              (should-not (plist-member focused :activity-records))
              (should (= (plist-get focused :tool-count) 1)))))
      (e-chat-owner-test--kill-buffer buffer))))

(ert-deftest e-chat-activity-owner-orphaned-progress-interval-settles ()
  "An interval whose owner handle was detached terminates itself."
  (let ((buffer (e-chat-activity-test--buffer))
        handle timer)
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-activity-start-progress "turn-1")
          (setq handle e-chat-activity--progress-interval-handle
                timer (plist-get (e-work-handle-metadata handle) :timer)
                e-chat-activity--progress-interval-handle nil)
          (funcall (timer--function timer))
          (should (e-request-terminal-p
                   (e-work-handle-lifecycle handle))))
      (when (timerp timer)
        (cancel-timer timer))
      (e-chat-owner-test--kill-buffer buffer))))

(ert-deftest e-chat-activity-owner-late-progress-tick-reports-stall ()
  "A delayed progress tick reports a local Emacs stall through surface status."
  (let ((buffer (e-chat-activity-test--buffer))
        (e-chat-progress-interval 0.5))
    (unwind-protect
        (with-current-buffer buffer
          (setq e-chat-activity--progress-turn-id "turn-1"
                e-chat-activity--progress-next-tick-time
                (- (float-time) 7.0))
          (cl-letf (((symbol-function
                      'e-chat-activity--stale-progress-turn-p)
                     (lambda (_turn-id) nil)))
            (e-chat-activity--advance-progress-indicator))
          (should (string-match-p
                   "Emacs was blocked for [0-9]+s; checking turn state"
                   (e-chat-surface-status))))
      (e-chat-owner-test--kill-buffer buffer))))

(provide 'e-chat-activity-test)

;;; e-chat-activity-test.el ends here

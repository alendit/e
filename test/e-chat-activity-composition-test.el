;;; e-chat-activity-composition-test.el --- Public chat activity composition tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Progress, reasoning, tool/action, transient, and redraw composition scenarios.

;;; Code:

(require 'ert)
(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-chat-test-progress-rerender-touches-only-active-tail ()
  "Progress redraw leaves completed activity rounds physically untouched."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-active-tail")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-test--mark-active-turn "turn-1")
          (dotimes (index 24)
            (let ((started-at (* index 2)))
              (e-chat-render-event
               (e-events-make :type 'provider-request-started
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :created-at started-at
                              :payload '(:status started)))
              (unless (= index 23)
                (e-chat-render-event
                 (e-events-make :type 'provider-request-finished
                                :session-id e-chat-session-id
                                :turn-id "turn-1"
                                :created-at (1+ started-at)
                                :payload '(:status done))))))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)))
          (let* ((tail-start (car (e-chat-surface-running-status-bounds)))
                 (stable-prefix
                  (buffer-substring-no-properties (point-min) tail-start))
                 changes)
            (add-hook 'before-change-functions
                      (lambda (start end)
                        (push (cons start end) changes))
                      nil t)
            (cl-letf (((symbol-function 'float-time)
                       (lambda (&optional _time) 60.0)))
              (e-chat-activity-advance-progress)
              (e-ui-work-with-batch-drain
                (e-ui-work-drain-batch :buffer (current-buffer))))
            (should changes)
            (should (cl-every (lambda (change)
                                (>= (car change) tail-start))
                              changes))
            (should (equal stable-prefix
                           (buffer-substring-no-properties
                            (point-min) tail-start)))
            (should (string-match-p "Thinking for 0min 14sec"
                                    (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-replayed-stale-provider-activity-stays-off-tail ()
  "Replayed non-terminal provider activity is hidden when the turn is not active."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-chat-test--create-session
           store :id "chat-provider-stale-replay")
          (e-session-append-message
           store "chat-provider-stale-replay"
           '(:role user :content "inspect" :turn-id "turn-1"))
          (e-session-append-activity-event
           store "chat-provider-stale-replay" "turn-1" 'turn-started nil)
          (e-session-append-activity-event
           store "chat-provider-stale-replay" "turn-1" 'provider-request-started
           '(:status started))
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-provider-stale-replay"))
          (with-current-buffer buffer
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (let ((content (buffer-string)))
              (should (string-match-p "inspect" content))
            (should-not (string-match-p "Thinking for" content)))
            (should-not (e-chat-activity-progress-turn-id))
            (should-not (plist-get (e-chat-activity-progress-state)
                                   :interval-active-p))
            (with-current-buffer (e-chat-test--composer buffer)
              (should (e-chat-composer-active-p)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-preserves-scrollback-focus ()
  "Progress redraws preserve point and window focus when reading scrollback."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-scroll-focus"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-2"
                            :created-at 20))
            (e-chat-test--mark-active-turn "turn-2")
            (with-current-buffer (e-chat-test--composer buffer)
              (goto-char (point-max))
              (insert "follow-up draft"))
            (goto-char (point-min))
            (set-window-point window (point))
            (set-window-start window (point))
            (let ((before-point (point))
                  (before-window-point (window-point window))
                  (before-window-start (window-start window)))
              (e-chat-activity-advance-progress)
              (should (= (point) before-point))
              (should (= (window-point window) before-window-point))
              (should (= (window-start window) before-window-start))
              (with-current-buffer (e-chat-test--composer buffer)
                (should (e-chat-composer-active-p))
                (should (equal (e-chat-composer-text)
                               "follow-up draft"))))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-activity-rerender-keeps-running-status-tail ()
  "Activity redraws keep following output when focus was at the active tail."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-status-tail"))
        (window nil)
        (composer-window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (setq composer-window
                (with-current-buffer buffer
                  (e-chat-surface-display-composer window t)))
          (select-window composer-window)
          (with-current-buffer buffer
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 10))
            (e-chat-test--mark-active-turn "turn-1")
            (e-chat-render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 11))
            (e-chat-render-event
             (e-events-make :type 'reasoning-delta
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:content "first chunk")))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (e-chat-surface-show-latest-output)
            (e-chat-surface-set-window-output-follow window t)
            (let ((old-tail (cdr (e-chat-surface-running-status-bounds)))
                  (old-output (e-chat-surface-output-follow-position)))
              (should old-tail)
              (should (= (window-point window) old-output))
              (e-chat-render-event
               (e-events-make :type 'reasoning-delta
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :payload '(:content "\nsecond chunk")))
              (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
              (let ((new-tail (cdr (e-chat-surface-running-status-bounds)))
                    (new-output (e-chat-surface-output-follow-position)))
                (should new-tail)
                (should (> new-tail old-tail))
                (should (= (window-point window) new-output))))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-deferred-activity-redraw-preserves-scrollback-focus ()
  "Deferred activity redraw keeps point and window focus in scrollback."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-scroll-focus"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-2"
                            :created-at 20))
            (e-chat-render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-2"
                            :created-at 21
                            :payload '(:type tool-call
                                        :id "call-1"
                                        :name "read")))
            (goto-char (point-min))
            (set-window-point window (point))
            (set-window-start window (point))
            (e-chat-surface-set-window-output-follow window nil)
            (let ((before-window-point (window-point window))
                  (before-window-start (window-start window)))
              (e-chat-activity-run-pending-redraw)
              (should (= (window-point window) before-window-point))
              (should (= (window-start window) before-window-start))
              (should (string-match-p "1 tool call" (buffer-string))))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-failed-turn-clears-transient-before-next-prompt ()
  "A failed turn drops its transient activity so the next prompt renders.
Regression: after turn-failed the active \"Thinking...\"/\"Thought for ...\"
block and its separators lingered, and the next submitted prompt rendered into
the orphaned region and appeared to vanish."
  (let ((buffer (e-chat-test--buffer nil "chat-failed-then-prompt")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _time) 8.0)))
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1" :created-at 0))
            (e-chat-render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1" :created-at 0
                            :payload '(:status started)))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
          ;; The active thinking transient is on screen.
          (should (string-match-p "Thinking for" (buffer-string)))
          ;; Turn fails.
          (e-chat-render-event
           (e-events-make :type 'turn-failed
                          :session-id e-chat-session-id
                          :turn-id "turn-1" :created-at 9
                          :payload '(:error "boom")))
          ;; The live "Thinking" transient is gone; the failure entry plus the
          ;; settled duration summary are shown.  The summary persists exactly
          ;; like a normally-completed turn, so the abnormal end still reports
          ;; duration and tool-call counts.
          (let ((content (buffer-string)))
            (should (string-match-p "Turn failed: boom" content))
            (should (string-match-p "Turn took 0min 9sec\\." content))
            (should-not (string-match-p "Thinking for" content))
            (should-not (string-match-p "Thought for" content)))
          ;; The active progress indicator is genuinely stopped: a live progress
          ;; marker would make the next turn's redraw delete a region that
          ;; swallows freshly rendered content (the reported symptom).  The
          ;; running-status/transient markers now point at the persistent
          ;; summary, as they do for any settled turn, so they are not checked.
          (should-not (plist-get (e-chat-activity-progress-state) :turn-id))
          ;; A new prompt for a fresh turn renders and stays visible.
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-2" :created-at 10
                          :payload '(:message (:role user
                                               :content "retry please"))))
          (let ((content (buffer-string)))
            (should (string-match-p "retry please" content))
            (should-not (string-match-p "Thinking for" content))
            (should-not (string-match-p "Thought for" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-keeps-editable-composer-draft ()
  "Progress redraws keep the follow-up composer editable with draft text."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-editable-composer")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-active-p))
            (goto-char (point-max))
            (insert "follow-up draft"))
          (e-chat-activity-advance-progress)
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-active-p))
            (should (equal (e-chat-composer-text) "follow-up draft"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-preserves-context-reference ()
  "Progress redraws keep inline context reference properties in the composer."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-context-reference")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'activity-redraw))
          (let* ((composer (e-chat-test--composer buffer))
                 (reference
                  (with-current-buffer composer
                    (goto-char (point-max))
                    (insert "Review ")
                    (prog1
                        (e-chat-composer-insert-context-reference
                         '(:id "ref-1"
                           :uri "buffer://source"
                           :label "source:2"
                           :text "two"
                           :start-line 2
                           :end-line 2
                           :point-line 2))
                      (insert " before replying.")))))
            (e-chat-activity-advance-progress)
            (with-current-buffer composer
              (should (e-chat-composer-active-p))
              (let ((document (e-chat-composer-document)))
                (should (equal (plist-get document :text)
                               "Review <reference id=\"ref-1\" label=\"source:2\"> before replying."))
                (should (equal (plist-get document :references)
                               (list reference))))
              (goto-char (e-chat-composer-start-position))
              (search-forward "@[source:2]")
              (should (equal (get-text-property (match-beginning 0)
                                                'e-chat-context-reference)
                             reference)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-redraw-updates-without-composer-rebuild ()
  "Clean progress redraws update the running status without composer churn."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-no-composer-rebuild")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'activity-redraw))
          (let ((composer (e-chat-test--composer buffer))
                tick)
            (with-current-buffer composer
              (goto-char (point-max))
              (insert "follow-up draft")
              (setq tick (buffer-chars-modified-tick)))
            (e-chat-activity-advance-progress)
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (with-current-buffer composer
              (should (= (buffer-chars-modified-tick) tick))
              (should (e-chat-composer-active-p))
              (should (equal (e-chat-composer-text)
                             "follow-up draft")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-token-usage-events-update-mode-line-without-transcript ()
  "Token usage events update status without rendering system transcript blocks."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness :session-id "chat-token-usage-event")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
          (e-session-append-activity-event
           store
           e-chat-session-id
           "turn-1"
           'token-usage
           '(:input-tokens 54581
             :cached-input-tokens 30720
             :output-tokens 154
             :reasoning-output-tokens 0
             :total-tokens 54735))
          (e-chat-render-event
           (e-events-make :type 'token-usage
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:input-tokens 54581
                                     :cached-input-tokens 30720
                                     :output-tokens 154
                                     :reasoning-output-tokens 0
                                     :total-tokens 54735)))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'chat-mode-line-status))
          (should (equal (e-chat-surface-mode-line-status)
                         "e-chat gpt-5.5/high 21% (55k/258k tok)"))
          (should-not (string-match-p "token-usage" (buffer-string)))
          (should-not (string-match-p "Event:" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-running-activity-keeps-single-response-separator ()
  "A running turn keeps one user/assistant separator after final output."
  (let ((buffer (e-chat-test--buffer nil "chat-running-response-separator")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:message (:role user
                                                :content "inspect"))))
          (e-chat-render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type tool-call
                                      :id "call-1"
                                      :name "read")))
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "done"))))
          (should (stringp (e-chat-transcript-response-separator)))
          (should (= (e-chat-test--count-font-lock-face-runs
                      'e-chat-separator-face
                      (point-min)
                      (point-max))
                     1))
          (should-not (string-match-p "2 tool calls" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-abort-cancels-active-tool-request ()
  "The chat abort command cancels an active tool request."
  (let* ((tool-callbacks nil)
         (tool-cancelled nil)
         (backend
          (e-backend-create
           :name "chat-tool-abort"
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                           on-request-start)
              (ignore messages options on-error on-request-start)
              (funcall on-item
                       '(:type tool-call
                         :id "call-1"
                         :name "held-tool"
                         :arguments (:stated_purpose "Hold this tool call."
                                     :text "hi")))
              (funcall on-item '(:type done :reason tool-use))
              (funcall on-done '(:status done))
              nil))))
         (tools (e-tools-registry-create))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-tool-abort")))
    (e-tools-test-register
     tools
     :name "held-tool"
     :description "Hold."
     :start
     (cl-function
      (lambda (&key arguments on-done on-error on-request-start)
        (ignore arguments on-error)
        (setq tool-callbacks (list :on-done on-done))
        (let ((request
               (e-tools-request-create
                :cancel (lambda ()
                          (setq tool-cancelled t)
                          t))))
          (funcall on-request-start request)
          request))))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-harness-tools)
                     (lambda (_harness &optional _session-id _turn-id) tools)))
            (e-chat-submit "run held tool")
            (should (e-chat-test--wait-until (lambda () tool-callbacks) 1.0))
            (e-chat-abort)
            (funcall (plist-get tool-callbacks :on-done) "late result")
            (should (e-chat-test--wait-until
                     (lambda ()
                       (not (plist-get
                             (e-chat-service-state harness e-chat-session-id)
                             :active-turn)))
                     1.0))
            (should tool-cancelled)
            (should (e-chat-test--wait-until
                     (lambda ()
                       (string-match-p "Turn cancelled" (buffer-string)))
                     1.0))
            (should (equal (mapcar (lambda (message)
                                     (plist-get message :role))
                                   (e-chat-service-messages
                                    harness e-chat-session-id))
                           '(user)))
            (should (seq-some
                     (lambda (event)
                       (eq (plist-get event :event-type) 'turn-cancelled))
                     (e-chat-service-activity-events
                      harness e-chat-session-id)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-hides-transient-tool-entries ()
  "Tool progress stays out of the transcript after the turn settles."
  (let ((buffer (e-chat-test--buffer nil "chat-events")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-failed
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:error "boom")))
          (e-chat-render-event
           (e-events-make :type 'turn-cancelled
                          :session-id e-chat-session-id
                          :turn-id "turn-2"))
          (e-chat-render-event
           (e-events-make :type 'tool-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-3"
                          :payload '(:result (:status ok))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     (concat (regexp-quote (e-chat-transcript-system-glyph))
                             " System\nTurn failed: boom")
                     content))
            (should (string-match-p
                     (concat (regexp-quote (e-chat-transcript-system-glyph))
                             " System\nTurn cancelled")
                     content))
            (should-not (string-match-p "(:status ok)" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-turn-started-shows-moving-assistant-progress ()
  "An active assistant turn shows a protected moving glyph indicator."
  (let ((buffer (e-chat-test--buffer nil "chat-progress")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-test--mark-active-turn "turn-1")
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'activity-redraw))
          (let ((content (buffer-string)))
            (should (string-match-p
                     (concat (regexp-quote (e-chat-transcript-assistant-glyph))
                             " ⠋")
                     content))
            (goto-char (point-min))
            (search-forward "⠋")
            (should (get-text-property (1- (point)) 'read-only)))
          (e-chat-activity-advance-progress)
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (should (string-match-p
                   (concat (regexp-quote (e-chat-transcript-assistant-glyph))
                           " ⠙")
                   (buffer-string)))
          (should (>= e-chat-progress-interval 0.5))
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (should (equal (e-chat-activity-progress-turn-id) "turn-1"))
          (e-chat-render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11))
          (let ((content (buffer-string)))
            (should (string-match-p
                     (concat (regexp-quote (e-chat-transcript-assistant-glyph))
                             " Final answer.")
                     content))
            (should-not (string-match-p
                         (concat (regexp-quote (e-chat-transcript-assistant-glyph))
                                 " [⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]")
                         content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-provider-wait-and-stream-statuses ()
  "Provider lifecycle and stream events update the compact running status."
  (let ((buffer (e-chat-test--buffer nil "chat-provider-status")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:provider codex
                                      :transport url-retrieve
                                      :url-host "example.test"
                                      :url-path "/codex/responses"
                                      :timeout-seconds 180
                                      :status started)))
          (should (string-match-p "waiting for provider"
                                  header-line-format))
          (should (equal (e-chat-activity-progress-turn-id) "turn-1"))
          (e-chat-render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type reasoning-delta
                                      :content "thinking")))
          (should (string-match-p "reasoning" header-line-format))
          (e-chat-render-event
           (e-events-make :type 'assistant-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type assistant-delta
                                      :content "answer")))
          (should (string-match-p "streaming" header-line-format)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-turn-steered-schedules-visible-indicator ()
  "A steering event schedules a visible active-turn indicator."
  (let ((buffer (e-chat-test--buffer nil "chat-steered-indicator")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-render-event
           (e-events-make :type 'turn-steered
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:prompt-preview "focus on the count")))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'activity-redraw))
          (should (string-match-p "steered" header-line-format))
          (should (string-match-p "Steered: focus on the count"
                                  (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-updates-active-thinking-row ()
  "Progress redraw updates the active thinking row without duplicating it."
  (let ((buffer (e-chat-test--buffer nil "chat-active-thinking-rerender")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _time) 8.0)))
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0))
            (e-chat-test--mark-active-turn "turn-1")
            (e-chat-render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0
                            :payload '(:status started))))
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _time) 15.0)))
            (e-chat-activity-advance-progress)
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "⠙ Thinking for 0min 15sec" content))
            (should (= (e-chat-test--count-occurrences
                        "Thinking for" content)
                       1))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-updates-between-provider-and-tool ()
  "Progress redraw keeps counting after a provider settles within a live turn."
  (let ((buffer (e-chat-test--buffer nil "chat-between-step-progress")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-test--mark-active-turn "turn-1")
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:status done)))
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _time) 8.0)))
            (e-chat-activity-advance-progress)
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "⠙ Working for 0min 8sec" content))
            (should-not (string-match-p
                         "Thought for 0min 1sec" content)))
          (e-chat-activity-stop-progress "turn-1")
          (let ((content (buffer-string)))
            (should (string-match-p
                     "Thought for 0min 1sec" content))
            (should-not (string-match-p
                         "Working for" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-interval-stops-when-harness-turn-settled ()
  "Progress ticks stop when the harness no longer has the progress turn active."
  (let ((buffer (e-chat-test--buffer nil "chat-stale-progress-turn")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (puthash e-chat-session-id
                   '(:id "turn-1" :status done)
                   (e-harness-active-turns e-chat-harness))
          (e-chat-activity-advance-progress)
          (should-not (e-chat-activity-progress-turn-id))
          (should-not (plist-get (e-chat-activity-progress-state)
                                 :interval-active-p))
          (should-not (string-match-p "Thinking for" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-tool-count-renders-on-thinking-row ()
  "A round's tool count renders on the same line as its activity row.
Once a tool completes, the left cell settles back to \"Thought for ...\"."
  (let ((buffer (e-chat-test--buffer nil "chat-tool-count-thinking-row")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          (e-chat-render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:id "call-1" :name "read")))
          (e-chat-render-event
           (e-events-make :type 'tool-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload '(:id "call-1" :result "done")))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "Thought for 0min 10sec +1 tool call" content))
            (should-not (string-match-p
                         "Thought for 0min 10sec\n\n1 tool call"
                         content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-running-tool-row-names-tool-and-ticks ()
  "While a tool runs, the activity row shows a live spinner, name, and duration."
  (let ((buffer (e-chat-test--buffer nil "chat-running-tool-row")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          ;; Freeze the clock so the elapsed duration is deterministic.
          (cl-letf (((symbol-function 'float-time)
                     (lambda () 25)))
            (e-chat-render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 20
                            :payload '(:id "call-1" :name "bash")))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (let ((content (buffer-string)))
              ;; The running tool is named with its live elapsed time, and the
              ;; frozen "Thought for" text is gone from the row while it runs.
              (should (string-match-p "Running bash for 0min 5sec" content))
              (should (string-match-p "1 tool call" content))
              (should-not (string-match-p "Thought for" content))))
          ;; When the tool finishes, the row settles back to the thought text.
          (e-chat-render-event
           (e-events-make :type 'tool-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 30
                          :payload '(:id "call-1" :result "ok")))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (let ((content (buffer-string)))
            (should (string-match-p "Thought for 0min 10sec" content))
            (should-not (string-match-p "Running bash" content))
            ;; The finished tool's name and run duration stay on the summary
            ;; row (started at 20, finished at 30 -> 0min 10sec).
            (should (string-match-p "1 tool call (bash) for 0min 10sec"
                                    content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-activity-redraw-does-not-recenter ()
  "Deferred activity redraws do not enter redisplay through `recenter'."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-redraw-no-recenter"))
        (window nil)
        recenter-called)
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0))
            (e-chat-test--mark-active-turn "turn-1")
            (e-chat-render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0
                            :payload '(:status started)))
            (when-let ((bounds (e-chat-surface-running-status-bounds)))
              (goto-char (cdr bounds))
              (set-window-point window (point))))
            (cl-letf (((symbol-function 'recenter)
                       (lambda (&rest _ignored)
                         (setq recenter-called t))))
              (with-current-buffer buffer
                (e-chat-test--run-pending-ui-work
                 'activity-redraw "turn-1")))
          (should-not recenter-called))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-running-tool-row-collapses-parallel-tools ()
  "Several tools running at once collapse into a counted running row."
  (let ((buffer (e-chat-test--buffer nil "chat-running-tools-parallel")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          (dolist (tool '(("call-1" "read")
                          ("call-2" "grep")))
            (e-chat-render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 11
                            :payload (list :id (nth 0 tool)
                                           :name (nth 1 tool)))))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (let ((content (buffer-string)))
            (should (string-match-p "Running 2 tools (read, grep)" content))
            (should (string-match-p "2 tool calls" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-tool-progress-renders-on-running-tool-row ()
  "Streaming tool progress updates the running activity row."
  (let ((buffer (e-chat-test--buffer nil "chat-tool-progress-row")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:status done)))
          (e-chat-render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:id "call-1" :name "bash")))
          (e-chat-render-event
           (e-events-make :type 'tool-progress
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 2
                          :payload '(:tool-call-id "call-1"
                                     :bytes 128
                                     :lines 4
                                     :preview "installing\n")))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "1 tool call (bash), 128 bytes output" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-activity-rounds-have-subtle-separators ()
  "Multiple intermittent rounds are separated inside the activity block."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-round-separators")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 20
                          :payload '(:status started)))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (let ((content (buffer-string)))
            (should (string-match-p
                     (concat "Thought for 0min 10sec\n"
                             (make-string 64 ?┈)
                             "\n⠋ Thinking for")
                     content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-activity-separator-is-quieter-than-response-separator ()
  "Activity round dividers are quieter than the prompt/agent separator."
  (should (boundp 'e-chat-activity-separator))
  (should (equal (e-chat-transcript-response-separator)
                 (e-chat-composer-separator)))
  (should (equal e-chat-activity-separator
                 (make-string 64 ?┈)))
  (should-not (equal e-chat-activity-separator
                     (e-chat-transcript-response-separator))))

(ert-deftest e-chat-test-activity-separator-uses-dim-activity-face ()
  "Activity round dividers use a dim face inside the activity block."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-separator-face")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 20
                          :payload '(:status started)))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (goto-char (point-min))
          (search-forward e-chat-activity-separator)
          (should (eq (get-text-property (match-beginning 0) 'font-lock-face)
                      'e-chat-activity-separator-face)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-reasoning-has-space-after-thought-row ()
  "Reasoning text has a small visual gap after the thought row."
  (let ((buffer (e-chat-test--buffer nil "chat-reasoning-spacer")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _time) 8.0)))
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0))
            (e-chat-render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0
                            :payload '(:status started)))
            (e-chat-render-event
             (e-events-make :type 'reasoning-delta
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 1
                            :payload '(:content "planning")))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "⠋ Thinking for 0min 8sec\n\nplanning"
                     content))
            (should-not (string-match-p
                         "⠋ Thinking for 0min 8sec\nplanning"
                         content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-chat-activity-composition-test)

;;; e-chat-activity-composition-test.el ends here

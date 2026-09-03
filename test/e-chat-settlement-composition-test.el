;;; e-chat-settlement-composition-test.el --- Public chat settlement composition tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Settlement, status, usage, compaction, and board-activity composition scenarios.

;;; Code:

(require 'ert)
(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-chat-test-final-turn-collapses-progress-to-summary ()
  "Settled activity collapses to a navigable turn summary."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-summary"))
        (details-calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat-test-activity-details-text)
                     (lambda (&rest _args)
                       (setq details-calls (1+ details-calls))
                       "eager details")))
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
                            :created-at 63
                            :payload '(:status done)))
            (dotimes (index 2)
              (e-chat-render-event
               (e-events-make :type 'tool-started
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :created-at 64
                              :payload (list :type 'tool-call
                                             :id (format "call-%d" index)
                                             :name "read"))))
            (e-chat-render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 160
                            :payload '(:status started)))
            (e-chat-render-event
             (e-events-make :type 'provider-request-finished
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 205
                            :payload '(:status done)))
            (e-chat-render-event
             (e-events-make :type 'message-added
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 205
                            :payload '(:message (:role assistant
                                                  :content "Final answer."))))
            (e-chat-render-event
             (e-events-make :type 'turn-finished
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 205))
            (should (= details-calls 0)))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "Turn took 3min 25sec, 2 tool calls\\." content))
            (should (string-match-p
                     (concat (regexp-quote (e-chat-transcript-assistant-glyph))
                             " Final answer\\.\n\n"
                             "Turn took 3min 25sec, 2 tool calls\\.")
                     content))
            (should-not (string-match-p "Thought for 1min 3sec" content)))
          (e-chat-test--focus-block-containing "Turn took 3min 25sec")
          (should (eq (plist-get (e-chat-test--focused-block) :kind)
                      'activity-summary))
          (call-interactively #'e-chat-response-navigation-activate)
          (let ((content (buffer-string)))
            (should (string-match-p "Thought for 1min 3sec" content))
            (should (string-match-p "2 tool calls" content))
            (should (string-match-p "Thought for 0min 45sec" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-action-activity-renders-in-summary ()
  "Action activity events render in the settled turn summary."
  (let ((buffer (e-chat-test--buffer nil "chat-action-summary")))
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
           (e-events-make :type 'action-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:capability-id workspace-awareness
                                      :action :focus-buffer)))
          (e-chat-render-event
           (e-events-make :type 'action-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 2
                          :payload '(:capability-id workspace-awareness
                                      :action :focus-buffer
                                      :status ok
                                      :result (:content "focused"))))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 2
                          :payload '(:status done)))
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 2
                          :payload '(:message (:role assistant
                                                :content "Done."))))
          (e-chat-render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 2))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "Turn took 0min 2sec, 1 action\\." content))
            (should-not (string-match-p "1 tool call" content)))
          (e-chat-test--focus-block-containing "Turn took 0min 2sec")
          (call-interactively #'e-chat-response-navigation-activate)
          (let ((content (buffer-string)))
            (should (string-match-p
                     "Action: workspace-awareness/focus-buffer"
                     content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-replayed-active-provider-activity-restores-progress ()
  "Replayed active provider activity restores the running progress block."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "chat-provider-active-replay")
          (e-session-append-message
           store "chat-provider-active-replay"
           '(:role user :content "inspect" :turn-id "turn-1"))
          (e-session-append-activity-event
           store "chat-provider-active-replay" "turn-1" 'turn-started nil)
          (e-session-append-activity-event
           store "chat-provider-active-replay" "turn-1" 'provider-request-started
           '(:status started))
          (puthash "chat-provider-active-replay"
                   '(:id "turn-1" :status running)
                   (e-harness-active-turns harness))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "chat-provider-active-replay")
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-provider-active-replay"))
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'float-time)
                       (lambda (&optional _time) 8.0)))
              (e-ui-work-with-batch-drain
                (e-ui-work-drain-batch :buffer (current-buffer))))
            (let ((content (buffer-string)))
              (should (string-match-p
                       "Thinking for 0min [0-9]+sec" content)))
            (should (equal (e-chat-activity-progress-turn-id) "turn-1"))
            (should (plist-get (e-chat-activity-progress-state)
                               :interval-active-p))
            (with-current-buffer (e-chat-test--composer buffer)
              (should (e-chat-composer-active-p)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-activity-rerender-preserves-running-status-focus ()
  "Activity redraws preserve point/window focus inside active output."
  (let ((e-chat-activity-reasoning-visible-line-limit 20)
        (buffer (e-chat-test--buffer nil "chat-activity-status-focus"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
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
            (goto-char (point-min))
            (search-forward "first chunk")
            (backward-word 1)
            (set-window-point window (point))
            (set-window-start window (point))
            (e-chat-surface-set-window-output-follow window nil)
            (let ((before-point (point))
                  (before-window-point (window-point window))
                  (before-window-start (window-start window)))
              (e-chat-render-event
               (e-events-make :type 'reasoning-delta
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :payload '(:content "\nsecond chunk")))
              (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
              (should (= (point) before-point))
              (should (= (window-point window) before-window-point))
              (should (= (window-start window) before-window-start))
              (should (looking-at-p "chunk")))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-activity-rerender-does-not-delete-whole-status ()
  "Streamed activity redraws update changed status text without full deletion."
  (let ((e-chat-activity-reasoning-visible-line-limit 20)
        (buffer (e-chat-test--buffer nil "chat-activity-status-minimal-delete")))
    (unwind-protect
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
          (let ((bounds (e-chat-surface-running-status-bounds))
                (delete-region-fn (symbol-function 'delete-region))
                (full-status-deletes 0))
            (should bounds)
            (cl-letf (((symbol-function 'delete-region)
                       (lambda (start end)
                         (when (and (= start (car bounds))
                                    (= end (cdr bounds)))
                           (setq full-status-deletes
                                 (1+ full-status-deletes)))
                         (funcall delete-region-fn start end))))
              (e-chat-render-event
               (e-events-make :type 'reasoning-delta
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :payload '(:content "\nsecond chunk")))
              (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
            (should (= full-status-deletes 0))
            (should (string-match-p "first chunk\nsecond chunk"
                                    (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-running-activity-is-navigable-while-progress-active ()
  "Running activity summary blocks are navigable before the turn settles."
  (let ((buffer (e-chat-test--buffer nil "chat-running-activity-nav")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type tool-call
                                      :id "call-1"
                                      :name "read")))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (goto-char (point-min))
          (search-forward "1 tool call")
          (call-interactively #'e-chat-enter-response-navigation)
          (should (eq (plist-get (e-chat-test--focused-block) :kind)
                      'activity))
          (call-interactively #'e-chat-response-navigation-activate)
          (should e-chat-tool-list-mode)
          (with-current-buffer (e-chat-test--composer buffer)
            (should (e-chat-composer-active-p))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-preserves-response-navigation ()
  "Progress redraws keep response navigation focused on the same block."
  (let ((buffer (e-chat-test--buffer nil "chat-running-nav-preserve")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-2"
                          :created-at 20))
          (goto-char (point-min))
          (search-forward "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (let ((focused-block-id (e-chat-transcript-focused-block))
                (focused-point (point)))
            (e-chat-activity-advance-progress)
            (should e-chat-response-navigation-mode)
            (should (equal (e-chat-transcript-focused-block) focused-block-id))
            (should (= (point) focused-point))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-mode-line-status-prefers-provider-token-usage ()
  "Mode-line status uses provider token usage before estimated context size."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness :session-id "chat-mode-line-usage")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
          (e-session-append-activity-event
           store
           e-chat-session-id
           "turn-1"
           'token-usage
           '(:input-tokens 202598
             :cached-input-tokens 7552
             :output-tokens 419
             :reasoning-output-tokens 139
             :total-tokens 203017))
          (e-chat-surface-set-status "idle" t)
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'chat-mode-line-status))
          (should (equal (e-chat-surface-mode-line-status)
                         "e-chat gpt-5.5/high 78% (203k/258k tok)")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-token-usage-before-compaction-uses-context-estimate ()
  "After compaction, stale provider usage does not hide compacted context size."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (e-chat-context-token-estimate-bytes-per-token 1.0))
    (with-temp-buffer
      (e-chat-mode)
      (setq-local e-current-harness harness)
      (setq-local e-chat-harness harness)
      (setq-local e-chat-session-id "chat-compacted-usage")
      (e-chat-surface-set-redraw-visible t)
      (e-chat-test--create-session store :id e-chat-session-id)
      (e-session-append-message
       store
       e-chat-session-id
       (list :id "old"
             :role 'user
             :content (make-string 1000 ?x)))
      (e-session-append-message
       store
       e-chat-session-id
       '(:id "kept" :role user :content "kept suffix"))
      (e-session-append-activity-event
       store
       e-chat-session-id
       "turn-1"
       'token-usage
       '(:input-tokens 202598
         :cached-input-tokens 7552
         :output-tokens 419
         :reasoning-output-tokens 139
         :total-tokens 203017))
      (e-session-append-compaction
       store
       e-chat-session-id
       "summary"
       :first-kept-entry-id "kept")
      (e-chat-surface-set-status "idle" t)
      (e-ui-work-with-batch-drain
        (e-ui-work-drain-batch :buffer (current-buffer)
                               :owner 'chat-mode-line-status))
      (should (string-match-p "~[0-9]+ pct" mode-name))
      (should-not (string-match-p "203k/258k tok" mode-name)))))

(ert-deftest e-chat-test-set-status-skips-tool-option-materialization ()
  "Ordinary status updates avoid building full turn options."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-status-lightweight"))
         (turn-option-calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-harness-turn-options)
                     (lambda (&rest _args)
                       (setq turn-option-calls (1+ turn-option-calls))
                       (error "full turn options should be skipped"))))
            (e-chat-surface-set-status "waiting for provider")
            (e-chat-surface-set-status "done"))
          (should (= turn-option-calls 0))
          (should (string-match-p "gpt-5.5/high" header-line-format)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-token-usage-event-skips-tool-option-materialization ()
  "Fresh token-usage mode-line refresh avoids full tool option materialization."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-token-usage-lightweight"))
         (turn-option-calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
          (e-session-append-activity-event
           store
           e-chat-session-id
           "turn-1"
           'token-usage
           '(:input-tokens 1200 :total-tokens 1300))
          (cl-letf (((symbol-function 'e-harness-turn-options)
                     (lambda (&rest _args)
                       (setq turn-option-calls (1+ turn-option-calls))
                       (error "full turn options should be skipped"))))
            (e-chat-render-event
             (e-events-make :type 'token-usage
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:input-tokens 1200
                                       :total-tokens 1300))))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'chat-mode-line-status))
          (should (= turn-option-calls 0))
          (should (string-match-p "1.2k/258k tok" mode-name)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-compaction-finished-refreshes-context-estimate ()
  "Finished compactions immediately refresh stale context estimates."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (e-chat-context-token-estimate-bytes-per-token 1.0)
         (buffer (e-chat-open :harness harness
                              :session-id "chat-compaction-refresh")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
          (e-session-append-message
           store
           e-chat-session-id
           (list :id "old"
                 :role 'user
                 :content (make-string 1000 ?x)))
          (e-session-append-message
           store
           e-chat-session-id
           '(:id "kept" :role user :content "kept suffix"))
          (e-chat-surface-set-status "idle" t)
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)
                                   :owner 'chat-mode-line-status))
          (should (string-match-p "~[0-9]+ pct" mode-name))
          (let ((before mode-name))
            (e-session-append-compaction
             store
             e-chat-session-id
             "summary"
             :first-kept-entry-id "kept")
            (e-chat-render-event
             (e-events-make :type 'compaction-finished
                            :session-id e-chat-session-id
                            :turn-id "turn-compact"
                            :payload '(:compaction-id "compaction-1"
                                       :first-kept-entry-id "kept")))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)
                                     :owner 'chat-mode-line-status))
            (should (string-match-p "~[0-9]+ pct" mode-name))
            (should-not (equal mode-name before))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-compact-session-command-renders_activity ()
  "Manual compaction command renders visible progress and writes a summary."
  (let* ((backend (e-backend-create
                   :name 'summary
                   :stream
                   (cl-function
                    (lambda (&key messages options on-item)
                      (ignore messages options)
                      (funcall on-item
                               '(:type assistant-message
                                 :content "Compacted summary."))))))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness :session-id "chat-compact")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((store (e-harness-sessions e-chat-harness)))
            (e-session-append-message store e-chat-session-id
                                      '(:role user :content "old"))
            (e-session-append-message store e-chat-session-id
                                      '(:role assistant :content "old answer"))
            (e-session-append-message store e-chat-session-id
                                      '(:role user :content "new"))
            (e-chat-compact-session)
            (should-not (e-session-compactions store e-chat-session-id))
            (should
             (e-chat-test--wait-until
              (lambda ()
                (e-session-compactions store e-chat-session-id))))
            (should (equal (plist-get
                            (car (e-session-compactions
                                  store e-chat-session-id))
                            :summary)
                           "Compacted summary."))
            (should (e-chat-test--wait-until
                     (lambda ()
                       (and (string-match-p "Context compaction started"
                                            (buffer-string))
                            (string-match-p "Context compacted into"
                                            (buffer-string))))
                     1.0))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-auto-compaction-renders-distinct-label ()
  "Auto-compaction events render with a distinct visible label."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness :session-id "chat-auto-label")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((store (e-harness-sessions e-chat-harness)))
            (e-harness-activity-emit-turn-event
             e-chat-harness e-chat-session-id "turn-auto" 'compaction-started
             '(:reason auto))
            (e-session-append-compaction
             store e-chat-session-id "summary"
             :metadata '(:reason auto))
            (e-harness-activity-emit-turn-event
             e-chat-harness e-chat-session-id "turn-auto" 'compaction-finished
             '(:compaction-id "compaction-auto" :reason auto))
            (should (e-chat-test--wait-until
                     (lambda ()
                       (and (string-match-p "Auto-compaction started"
                                            (buffer-string))
                            (string-match-p "Auto-compacted context into"
                                            (buffer-string))))
                     1.0))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-board-progress-uses-presentation-turn-identity ()
  "Board-private turn ids do not make presentation progress look stale."
  (let ((buffer (e-chat-test--buffer nil "chat-board-progress-identity"))
        harness
        session-id)
    (unwind-protect
        (with-current-buffer buffer
          (setq harness e-chat-harness
                session-id e-chat-session-id)
          (let* ((binding (e-chat-service-binding harness session-id))
                 (attachment (e-chat-service-binding-attachment binding))
                 (participant-id
                  (e-board-registry-participant-id
                   (e-board-runtime-attachment-participant attachment)))
                 (board (e-board-registry-board-source-board
                         (e-chat-service-binding-board binding)))
                 (presentation-turn-id "msg-progress-input")
                 (source-turn-id '(board participant source-turn))
                 (author (format "participant:%s" participant-id)))
            (e-board-post-input
             board :id presentation-turn-id :author "test-client" :tags '(main)
             :content "inspect" :source-input-key '(test-progress 1 0))
            (puthash session-id
                     (list :id source-turn-id :status 'running)
                     (e-harness-active-turns harness))
            (e-board-post-activity
             board :id "progress-turn-started" :author author
             :subject-participant-id participant-id
             :source-turn-id source-turn-id :activity-kind 'turn-started
             :tags '(main) :reply-to-message-ids (list presentation-turn-id)
             :source-activity-key '(test-progress 1 1))
            (e-board-post-activity
             board :id "progress-provider-started" :author author
             :subject-participant-id participant-id
             :source-turn-id source-turn-id
             :activity-kind 'provider-request-started :tags '(main)
             :attributes '(:status started)
             :reply-to-message-ids (list presentation-turn-id)
             :source-activity-key '(test-progress 1 2))
            (e-board-post-activity
             board :id "progress-provider-finished" :author author
             :subject-participant-id participant-id
             :source-turn-id source-turn-id
             :activity-kind 'provider-request-finished :tags '(main)
             :attributes '(:status done)
             :reply-to-message-ids (list presentation-turn-id)
             :source-activity-key '(test-progress 1 3))
            (e-chat-test--dispatch-observed-event
             (list :type 'turn-started
                   :session-id session-id
                   :turn-id presentation-turn-id
                   :board-id (e-board-registry-board-id
                              (e-chat-service-binding-board binding))
                   :subject-participant-id participant-id
                   :selected-participant-p t
                   :source-turn-id source-turn-id
                   :created-at 0))
            (e-chat-test--dispatch-observed-event
             (list :type 'provider-request-started
                   :session-id session-id
                   :turn-id presentation-turn-id
                   :board-id (e-board-registry-board-id
                              (e-chat-service-binding-board binding))
                   :subject-participant-id participant-id
                   :selected-participant-p t
                   :source-turn-id source-turn-id
                   :created-at 1
                   :payload '(:status started)))
            (e-chat-test--dispatch-observed-event
             (list :type 'provider-request-finished
                   :session-id session-id
                   :turn-id presentation-turn-id
                   :board-id (e-board-registry-board-id
                              (e-chat-service-binding-board binding))
                   :subject-participant-id participant-id
                   :selected-participant-p t
                   :source-turn-id source-turn-id
                   :created-at 2
                   :payload '(:status done)))
            (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
            (should (equal (e-chat-activity-progress-turn-id)
                           presentation-turn-id))
            (should
             (equal
              (plist-get
               (e-chat-service-active-turn harness session-id)
               :id)
              presentation-turn-id))
            (should
             (equal
              (plist-get
               (plist-get (e-chat-service-state harness session-id)
                          :active-turn)
               :id)
              presentation-turn-id))
            (should
             (equal
              (plist-get
               (gethash session-id
                        (e-chat-service-active-turns harness))
               :id)
              presentation-turn-id))
            (cl-letf (((symbol-function 'float-time)
                       (lambda (&optional _time) 8.0)))
              (e-chat-activity-advance-progress)
              (e-ui-work-with-batch-drain
                (e-ui-work-drain-batch :buffer (current-buffer))))
            (should (equal (e-chat-activity-progress-turn-id)
                           presentation-turn-id))
            (should (plist-get (e-chat-activity-progress-state)
                               :interval-active-p))
            (should (string-match-p
                     "Working for [0-9]+min [0-9]+sec" (buffer-string)))))
      (when (and harness session-id)
        (remhash session-id (e-harness-active-turns harness)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-chat-settlement-composition-test)

;;; e-chat-settlement-composition-test.el ends here

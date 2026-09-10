;;; e-chat-transcript-mechanism-test.el --- Transcript owner mechanism tests -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests exercise transcript-owned private mechanisms through the
;; composed shell fixture.  Public composed behavior remains in
;; `e-chat-presentation-integration-test.el'; standalone transcript contracts
;; live in `e-chat-transcript-test.el'.

;;; Code:

(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-chat-test-final-claim-message-renders-and-expands-details ()
  "A final claim stays intact, advertises details, and expands with RET."
  (let ((buffer (e-chat-test--buffer nil "chat-final-claim-details"))
        (content
         (concat
          "Yes. It used the Bayesian `#+begin_reasoning` structure.\n\n"
          "#+begin_reasoning\n"
          "claim: The daily was populated\n"
          "confidence: high\n"
          "alternatives: stale note state\n"
          "evidence: src:ABCDEF12\n"
          "#+end_reasoning\n")))
    (unwind-protect
        (with-current-buffer buffer
          (e-harness-activate-capability
           e-chat-harness (e-bayesian-reasoning-capability-create))
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1" :created-at 10))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1" :created-at 10
                          :payload nil))
          (e-chat-render-event
           (e-events-make
            :type 'message-added :session-id e-chat-session-id
            :turn-id "turn-1" :created-at 22
            :payload (list :message
                           (list :id "message-1" :role 'assistant
                                 :turn-id "turn-1" :content content))))
          (e-chat-render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1" :created-at 22))
          (let ((rendered (buffer-string)))
            (should (string-match-p
                     (regexp-quote
                      "Yes. It used the Bayesian `#+begin_reasoning` structure.")
                     rendered))
            (should-not (string-match-p
                         (regexp-quote "\n#+begin_reasoning\n") rendered))
            (should (string-match-p
                     (regexp-quote "Turn took 0min 12sec (1 claim).")
                     rendered)))
          (e-chat-test--focus-block-containing "Yes. It used the Bayesian")
          (let ((block (e-chat-transcript--focused-block)))
            (should (eq (plist-get block :kind) 'final))
            (e-chat-response-navigation-activate)
            (should (e-chat-transcript--block-details-visible-p block))
            (should (string-match-p "Claims\n- The daily was populated"
                                    (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-composed-progress-follows-tail-viewports-only ()
  "Composer focus follows a tailing transcript but preserves scrollback."
  (let* ((history (mapconcat (lambda (number)
                               (format "history line %d" number))
                             (number-sequence 1 300)
                             "\n"))
         (buffer (e-chat-test--buffer nil "chat-composed-progress-viewport"))
         transcript-window composer-window composer)
    (unwind-protect
        (progn
          (setq transcript-window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat-test--render-turn "history" 1 2 "Earlier" history)
            (setq composer-window
                  (e-chat-surface-display-composer transcript-window t))
            (setq composer (window-buffer composer-window))
            ;; Selecting the input pane changes `current-buffer'; transcript
            ;; events still render in its owning transcript buffer.
            (set-buffer buffer)
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
                            :payload '(:content "first progress")))
            (e-chat-activity-cancel-pending-redraw)
            (e-chat-activity-render-turn-transient "turn-1")
            (e-chat-surface-show-latest-output)
            ;; The normal UI redraw does this before the next scheduled
            ;; update.  Batch ERT has no redisplay loop of its own.
            (redisplay t)
            (let ((old-tail (cdr (e-chat-transcript--activity-bounds))))
              (should (eq (selected-window) composer-window))
              (should (eq (window-buffer (selected-window)) composer))
              (should (>= (window-end transcript-window t) old-tail))
              (e-chat-render-event
               (e-events-make :type 'reasoning-delta
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :payload '(:content "\nsecond progress")))
              (e-chat-activity-cancel-pending-redraw)
              (e-chat-activity-render-turn-transient "turn-1")
              (should (= (window-point transcript-window)
                         (cdr (e-chat-transcript--activity-bounds))))
              (should (eq (selected-window) composer-window))
              (with-current-buffer composer
                (e-chat-surface-capture-selected-output-follow-command))
              (with-selected-window transcript-window
                (goto-char (point-min))
                (set-window-point transcript-window (point))
                (set-window-start transcript-window (point)))
              (redisplay t)
              (should (eq (selected-window) composer-window))
              ;; `scroll-other-window' and mouse-wheel commands can move the
              ;; paired transcript while the composer remains selected.  Its
              ;; normal post-command boundary must observe that viewport.
              (with-current-buffer composer
                (e-chat-surface-post-command)
                (should-not
                 (e-chat-surface-window-follows-output-p transcript-window)))
              (should-not
               (e-chat-surface-window-follows-output-p transcript-window))
              (e-chat-render-event
               (e-events-make :type 'reasoning-delta
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :payload '(:content "\nthird progress")))
              (e-chat-activity-cancel-pending-redraw)
              (e-chat-activity-render-turn-transient "turn-1")
              (should (= (window-start transcript-window) (point-min)))
              (should (= (window-point transcript-window) (point-min)))
              (should (eq (selected-window) composer-window))
              ;; Returning the paired transcript to its output tail repins it,
              ;; even though the composer remains the selected constituent.
              (with-current-buffer composer
                (e-chat-surface-capture-selected-output-follow-command))
              (set-window-start
               transcript-window (car (e-chat-transcript--activity-bounds)))
              (cl-letf (((symbol-function 'e-chat-surface-window-reaches-output-p)
                         (lambda (window _tail)
                           (eq window transcript-window))))
                (with-current-buffer composer
                  (e-chat-surface-post-command)))
              (should (e-chat-surface-window-follows-output-p transcript-window))
              (e-chat-render-event
               (e-events-make :type 'reasoning-delta
                              :session-id e-chat-session-id
                              :turn-id "turn-1"
                              :payload '(:content "\nfourth progress")))
              (e-chat-activity-cancel-pending-redraw)
              (e-chat-activity-render-turn-transient "turn-1")
              (should (= (window-point transcript-window)
                         (cdr (e-chat-transcript--activity-bounds))))
              (should (eq (selected-window) composer-window))))
      (when (window-live-p composer-window)
        (delete-window composer-window))
      (when (window-live-p transcript-window)
        (delete-window transcript-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))))





(ert-deftest e-chat-test-tool-activity-compacts-large-result-display ()
  "Chat activity keeps a bounded tool result preview."
  (let ((buffer (e-chat-test--buffer nil "chat-tool-activity-preview")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((e-chat-tool-activity-preview-bytes 8))
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"))
            (e-chat-render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:id "call-1" :name "bash")))
            (e-chat-render-event
             (e-events-make :type 'tool-finished
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload
                            (list :tool-call '(:id "call-1" :name "bash")
                                  :result
                                  (list :tool-call-id "call-1"
                                        :name "bash"
                                        :status 'ok
                                        :content (make-string 64 ?x)
                                        :metadata '(:tmp-uri "tmp://full.txt"))))))
          (let* ((display (e-chat-activity-turn-display "turn-1"))
                 (item (car (plist-get display :tool-items)))
                 (output (plist-get item :output)))
            (should (string-match-p "xxxxxxxx" output))
            (should (string-match-p "Tool result preview truncated" output))
            (should (string-match-p "tmp://full.txt" output))
            (should-not (string-match-p (make-string 32 ?x) output))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-response-navigation-starts-at-latest-from-composer ()
  "Response navigation starts at the latest turn when point is in the composer."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-latest")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 23.5 "second" "two")
          (goto-char (point-max))
          (call-interactively #'e-chat-enter-response-navigation)
          (should e-chat-response-navigation-mode)
          (should (equal (plist-get (e-chat-transcript-focused-block) :turn-id)
                         "turn-2"))
          (should (string-match-p
                   (concat (regexp-quote (e-chat-transcript-assistant-glyph)) " two")
                   (e-chat-test--focused-turn-text)))
          (should-not (string-match-p
                       (concat (regexp-quote (e-chat-transcript-user-glyph)) " second")
                       (e-chat-test--focused-turn-text))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-render-session-skips-hidden-messages ()
  "A message flagged `:display' `hidden' is invisible in the clean transcript.
A superseded first attempt stays in the store for audit but the shell shows
only the visible reply while retaining a durable-to-rendered projection."
  (let ((buffer (e-chat-test--buffer nil "chat-hidden-render")))
    (unwind-protect
        (with-current-buffer buffer
          (e-session-append-message
           (e-harness-sessions e-chat-harness) "chat-hidden-render"
           (list :id "m-visible" :role 'assistant :turn-id "turn-1"
                 :content "visible answer"))
          (e-session-append-message
           (e-harness-sessions e-chat-harness) "chat-hidden-render"
           (list :id "m-hidden" :role 'assistant :turn-id "turn-1"
                 :content "superseded first attempt" :display 'hidden))
          (e-chat-test--seed-board-log-from-private-fixture
           e-chat-harness e-chat-session-id)
          (e-chat-clear)
          (e-chat-transcript-render-session)
          (let* ((visible-id (e-chat-transcript--message-block-id "m-visible"))
                 (hidden-id (e-chat-transcript--message-block-id "m-hidden")))
            (should visible-id)
            (should hidden-id)
            (should (e-chat-transcript--live-block-record visible-id))
            (should-not (e-chat-transcript--live-block-record hidden-id))
            (should (e-chat-test--message-display-hidden-p "m-hidden"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-message-updated-reconciles-rendered-reply-locally ()
  "Flipping a rendered message hidden/visible updates its block without replay.
The bayesian follow-up hides the first attempt after it was already shown, so
the shell must react to the `message-updated' event without rebuilding a long
transcript."
  (let ((buffer (e-chat-test--buffer nil "chat-hidden-update")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-surface-set-redraw-visible t)
          (let ((message (list :id "message-1" :role 'assistant
                               :content "first attempt reply")))
            (e-chat-render-event
             (e-events-make :type 'message-added
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload (list :message message)))
            (should (string-match-p "first attempt reply" (buffer-string)))
            (let ((rerenders 0)
                  (message-id (plist-get message :id)))
              (cl-letf (((symbol-function 'e-chat-transcript-rerender)
                         (lambda () (setq rerenders (1+ rerenders)))) )
                (e-chat-render-event
                 (e-events-make
                  :type 'message-updated :session-id e-chat-session-id
                  :turn-id "turn-1"
                  :payload (list :message
                                 (plist-put (copy-sequence message)
                                            :display 'hidden))))
                (let ((block-id (e-chat-transcript--message-block-id message-id)))
                  (should block-id)
                  (should-not (e-chat-transcript--live-block-record block-id))
                  (should (e-chat-test--message-display-hidden-p message-id)))
                (e-chat-render-event
                 (e-events-make
                  :type 'message-updated :session-id e-chat-session-id
                  :turn-id "turn-1"
                  :payload (list :message message)))
                (let ((block-id (e-chat-transcript--message-block-id message-id)))
                  (should (e-chat-transcript--live-block-record block-id))
                  (should-not (e-chat-test--message-display-hidden-p message-id)))
                (should (= rerenders 0))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-replayed-sibling-terminal-events-remain-observable ()
  "Replay renders sibling failures and cancellations without selected settlement."
  (let ((buffer (e-chat-test--buffer nil "chat-replayed-siblings")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "selected-turn"
                            :created-at 0))
          (let ((selected-status (e-chat-surface-status))
                (activity-events
                 (list
                  '(:turn-id "selected-turn" :event-type turn-failed
                    :created-at 1 :board-id "board-1"
                    :subject-participant-id "participant-sibling"
                    :source-turn-id "sibling-failure"
                    :selected-participant-p nil
                    :payload (:error "replayed sibling failure"))
                  '(:turn-id "selected-turn" :event-type turn-cancelled
                    :created-at 2 :board-id "board-1"
                    :subject-participant-id "participant-sibling"
                    :source-turn-id "sibling-cancel"
                    :selected-participant-p nil))))
            (dolist (event activity-events)
              (e-chat-activity-handle-event
               (plist-put (copy-sequence event)
                          :type (plist-get event :event-type))))
            (should (string-match-p "replayed sibling failure"
                                    (buffer-string)))
            (should (string-match-p "Turn cancelled" (buffer-string)))
            (should (e-chat-activity-turn-display
                     (e-chat-transcript-observed-turn-id
                      "selected-turn" (car activity-events))))
            (should (e-chat-activity-turn-display
                     (e-chat-transcript-observed-turn-id
                      "selected-turn" (cadr activity-events))))
            (should (equal (e-chat-surface-status) selected-status))
            (should (equal (e-chat-activity-progress-turn-id)
                           "selected-turn"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-long-final-response-defers-markdown-presentation ()
  "Long assistant text appears before deferred Markdown presentation runs."
  (let ((buffer (e-chat-test--buffer nil "chat-final-markdown-deferred"))
        (e-chat-deferred-markdown-threshold-bytes 8))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-transcript-insert-entry
           "Assistant"
           "Use **bold** and `code`."
           nil
           "turn-final-md")
          (goto-char (point-min))
          (search-forward "bold")
          (let ((faces (ensure-list
                        (get-text-property (1- (point)) 'face))))
            (should (memq 'e-chat-final-assistant-face faces))
            (should-not (memq 'e-chat-markdown-strong-face faces)))
          (let ((pending (e-chat-test--pending-ui-work
                          'markdown-presentation)))
            (should (= (length pending) 1))
            (should (equal (plist-get (car pending) :key)
                           e-chat-transcript--markdown-presentation-generation))
            (should (e-chat-test--live-work-handle-p
                     (plist-get (car pending) :handle))))
          (should (e-chat-test--wait-until
                   (lambda ()
                     (not (e-chat-test--pending-ui-work
                           'markdown-presentation)))))
          (goto-char (point-min))
          (search-forward "bold")
          (let ((faces (ensure-list
                        (get-text-property (1- (point)) 'face))))
            (should (memq 'e-chat-final-assistant-face faces))
            (should (memq 'e-chat-markdown-strong-face faces)))
          (search-forward "code")
          (let ((faces (ensure-list
                        (get-text-property (1- (point)) 'face))))
            (should (memq 'e-chat-final-assistant-face faces))
            (should (memq 'e-chat-markdown-code-face faces))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-deferred-markdown-renders-in-line-chunks ()
  "Deferred Markdown presentation honors the configured line chunk budget."
  (let ((buffer (e-chat-test--buffer nil "chat-final-markdown-chunks"))
        (e-chat-deferred-markdown-threshold-bytes 8)
        (e-chat-deferred-markdown-chunk-lines 1)
        (chunks 0))
    (unwind-protect
        (with-current-buffer buffer
          (let ((original (symbol-function 'e-chat-transcript--apply-markdown-line-faces)))
            (cl-letf (((symbol-function 'e-chat-transcript--apply-markdown-line-faces)
                       (lambda (start end)
                         (setq chunks (1+ chunks))
                         (funcall original start end))))
              (e-chat-transcript-insert-entry
               "Assistant"
               "Use **one**\nUse **two**\nUse **three**.")
              (should (e-chat-test--pending-ui-work
                       'markdown-presentation))
              (should
               (e-chat-test--wait-until
                (lambda ()
                  (not (e-chat-test--pending-ui-work
                        'markdown-presentation)))))
              (should (> chunks 1))
              (goto-char (point-min))
              (search-forward "three")
              (let ((faces (ensure-list
                            (get-text-property
                             (match-beginning 0) 'face))))
                (should (memq 'e-chat-markdown-strong-face faces))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-deferred-markdown-cancels-on-clear ()
  "Deferred Markdown callbacks do not apply to stale cleared buffers."
  (let ((buffer (e-chat-test--buffer nil "chat-markdown-cancel"))
        (e-chat-deferred-markdown-threshold-bytes 8)
        (calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'e-chat-transcript--apply-assistant-markdown)
                     (lambda (&rest _args)
                       (setq calls (1+ calls)))))
            (e-chat-transcript-insert-entry
             "Assistant"
             "Use **bold** and `code`.")
            (should (e-chat-test--pending-ui-work
                     'markdown-presentation))
            (e-chat-clear)
            (accept-process-output nil 0.05)
            (should (= calls 0))
            (should-not (e-chat-test--pending-ui-work
                         'markdown-presentation))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-response-navigation-starts-at-turn-under-point ()
  "Response navigation starts at the rendered turn under point."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-under-point")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 23.5 "second" "two")
          (goto-char (point-min))
          (search-forward "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (should (equal (plist-get (e-chat-transcript-focused-block) :turn-id)
                         "turn-1"))
          (should (string-match-p
                   (concat (regexp-quote (e-chat-transcript-assistant-glyph)) " one")
                   (e-chat-test--focused-turn-text)))
          (should-not (string-match-p
                       (concat (regexp-quote (e-chat-transcript-user-glyph)) " first")
                       (e-chat-test--focused-turn-text))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-block-view-gg-and-g-move-within-focused-block ()
  "Block view gg/G move to the focused block content text bounds."
  (let ((buffer (e-chat-test--buffer nil "chat-block-view-goto")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one\ntwo\nthree")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (let ((bounds (e-chat-transcript--block-content-bounds
                         (e-chat-transcript--block-view-block))))
            (goto-char (+ (car bounds) 4))
            (call-interactively
             (lookup-key e-chat-block-view-mode-map (kbd "G")))
            (should (= (point) (cdr bounds)))
            (should (equal (char-before) ?e))
            (should (equal (char-after) ?\n))
            (call-interactively
             (lookup-key e-chat-block-view-mode-map (kbd "g g")))
            (should (= (point) (car bounds)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-failed-turn-expands-full-error-inline ()
  "RET on a focused failed system block expands provider details inline."
  (let ((buffer (e-chat-test--buffer nil "chat-failed-details")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make
            :type 'turn-failed
            :session-id e-chat-session-id
            :turn-id "turn-1"
            :created-at 10
            :payload
            '(:error "OpenAI request failed: (error http 400)"
              :details (:type "response.failed"
                        :response
                        (:error
                         (:code "context_length_exceeded"
                          :message
                          "Your input exceeds the context window."))))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "Turn failed: OpenAI request failed: (error http 400)"
                     content))
            (should-not (string-match-p "context_length_exceeded" content)))
          (e-chat-test--focus-block-containing "Turn failed")
          (should (eq (plist-get (e-chat-test--focused-block) :kind)
                      'system))
          (e-chat-response-navigation-activate)
          (let ((content (buffer-string))
                (block (e-chat-test--focused-block)))
            (should e-chat-block-view-mode)
            (should-not e-chat-response-navigation-mode)
            (should (plist-get block :details-visible-p))
            (should-not (get-buffer e-chat-details-buffer-name))
            (should (string-match-p "OpenAI request failed" content))
            (should (string-match-p "context_length_exceeded" content))
            (should (string-match-p
                     "Your input exceeds the context window"
                     content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-active-activity-restores-missing-progress-interval ()
  "Rendering active activity restarts a missing progress interval."
  (let ((buffer (e-chat-test--buffer nil "chat-active-thinking-interval-restore")))
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
          (e-chat-activity-stop-progress)
          (let ((record (e-chat-transcript--existing-turn-record "turn-1")))
            (ignore record)
            (should (e-chat-activity-active-p "turn-1"))
            (cl-letf (((symbol-function 'float-time)
                       (lambda (&optional _time) 15.0)))
              (e-chat-activity-render-turn-transient "turn-1"))
            (should (equal (e-chat-activity-progress-turn-id) "turn-1"))
            (should (plist-get (e-chat-activity-progress-state)
                               :interval-active-p))
            (cl-letf (((symbol-function 'float-time)
                       (lambda (&optional _time) 16.0)))
              (e-chat-activity-advance-progress)
              (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer))))
            (let ((content (buffer-string)))
              (should (string-match-p "⠙ Thinking for 0min 16sec" content))
              (should (= (e-chat-test--count-occurrences
                          "Thinking for" content)
                         1)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-provider-error-settles-thinking-line ()
  "Provider errors turn open thinking into a failed thought line."
  (let ((buffer (e-chat-test--buffer nil "chat-provider-error-thinking")))
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
                          :created-at 12
                          :payload '(:status error)))
          (e-chat-render-event
           (e-events-make :type 'turn-failed
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload '(:error "provider failed")))
          (let ((display (e-chat-activity-turn-display "turn-1")))
            (should (equal (plist-get display :round-statuses) '(failed)))
            (should (string-match-p
                     "Thought failed after 0min 12sec"
                     (plist-get display :expanded-text))))
          (let ((content (buffer-string)))
            (should (string-match-p "Turn failed: provider failed" content))
            (should-not (string-match-p "Thinking\\.\\.\\." content))
            ;; The abnormal end still surfaces the settled duration summary.
            (should (string-match-p "Turn took 0min 12sec\\." content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-turn-cancelled-settles-thinking-line ()
  "Turn cancellation turns open thinking into a cancelled thought line."
  (let ((buffer (e-chat-test--buffer nil "chat-cancelled-thinking")))
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
           (e-events-make :type 'turn-cancelled
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12))
          (let ((display (e-chat-activity-turn-display "turn-1")))
            (should (equal (plist-get display :round-statuses) '(cancelled)))
            (should (string-match-p
                     "Thought cancelled after 0min 12sec"
                     (plist-get display :expanded-text))))
          (let ((content (buffer-string)))
            (should (string-match-p "Turn cancelled" content))
            (should-not (string-match-p "Thinking\\.\\.\\." content))
            ;; The abnormal end still surfaces the settled duration summary.
            (should (string-match-p "Turn took 0min 12sec\\." content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-retry-relabels-failed-provider-attempt-with-error ()
  "A retrying provider attempt is not presented as a failed thought."
  (let ((buffer (e-chat-test--buffer nil "chat-retrying-provider")))
    (unwind-protect
        (with-current-buffer buffer
          (dolist
              (event
               (list
                (e-events-make :type 'turn-started
                               :session-id e-chat-session-id
                               :turn-id "turn-1" :created-at 0)
                (e-events-make :type 'provider-request-started
                               :session-id e-chat-session-id
                               :turn-id "turn-1" :created-at 0)
                (e-events-make :type 'provider-request-finished
                               :session-id e-chat-session-id
                               :turn-id "turn-1" :created-at 5
                               :payload '(:status error))
                (e-events-make :type 'turn-retrying
                               :session-id e-chat-session-id
                               :turn-id "turn-1" :created-at 5
                               :payload '(:error "503: upstream unavailable"
                                          :details (:status 503)
                                          :attempt 1
                                          :reset-wait 1.5
                                          :backoff-seconds 2.0))))
            (e-chat-render-event event))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch :buffer (current-buffer)))
          (let* ((display (e-chat-activity-turn-display "turn-1"))
                 (expanded (plist-get display :expanded-text))
                 (content (buffer-string)))
            (should (equal (plist-get display :round-statuses) '(retrying)))
            (should (string-match-p
                     "Provider attempt failed after 0min 5sec; retry 1 in 2sec"
                     expanded))
            (should (string-match-p "Error: 503: upstream unavailable" expanded))
            (should-not (string-match-p "Thought failed" expanded))
            (should (string-match-p "retry 1 in 2s" content))
            (let ((details (plist-get display :details-text)))
              (should (string-match-p "Provider retry 1" details))
              (should (string-match-p "Retry delay: 2\\.0 seconds" details))
              (should (string-match-p "Reset wait: 1\\.5 seconds" details))
              (should (string-match-p
                       "Error: 503: upstream unavailable" details))
              (should (string-match-p "Provider details" details))
              (should (string-match-p "(:status 503)" details)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-intermediate-assistant-keeps-turn-progress-active ()
  "Assistant messages do not settle presentation before the terminal event."
  (let ((buffer (e-chat-test--buffer nil "chat-intermediate-assistant")))
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
                          :created-at 0))
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
                                                :content "First answer."))))
          (let ((display (e-chat-activity-turn-display "turn-1")))
            (should (equal (e-chat-activity-progress-turn-id) "turn-1"))
            (should-not (plist-get display :settled))
            (should (string-match-p "First answer" (buffer-string)))
            (should-not (string-match-p "Turn took" (buffer-string))))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 3))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 5
                          :payload '(:status done)))
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 5
                          :payload '(:message (:role assistant
                                                :content "Corrected answer."))))
          (should (equal (e-chat-activity-progress-turn-id) "turn-1"))
          (should-not (string-match-p "Turn took" (buffer-string)))
          (e-chat-render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 6
                          :payload '(:reason stop)))
          (let ((display (e-chat-activity-turn-display "turn-1"))
                (content (buffer-string)))
            (should-not (e-chat-activity-progress-turn-id))
            (should (plist-get display :settled))
            (should (= (save-excursion
                         (goto-char (point-min))
                         (how-many "Turn took" (point-min) (point-max)))
                       1))
            (should (string-match-p
                     (concat "Corrected answer\\.\n\n"
                             "Turn took 0min 6sec\\.")
                     content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-board-final-assistant-settles-turn-progress ()
  "Board-final assistant output settles presentation before its summary page."
  (let ((buffer (e-chat-test--buffer nil "chat-board-final-assistant")))
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
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 2
                          :payload
                          '(:message (:role assistant
                                      :content "Finished answer."
                                      :terminal-output t))))
          (let ((display (e-chat-activity-turn-display "turn-1"))
                (content (buffer-string)))
            (should-not (e-chat-activity-progress-turn-id))
            (should (plist-get display :settled))
            (should (string-match-p "Finished answer" content))
            (should-not (string-match-p "Thinking for" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-activity-summary-expands-to-child-blocks ()
  "Settled activity summary expansion creates navigable child blocks."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-summary-children")))
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
                          :created-at 0))
          (e-chat-render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:content "planning")))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 63
                          :payload '(:status done)))
          (e-chat-render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 64
                          :payload '(:id "call-1" :name "read")))
          (e-chat-render-event
           (e-events-make :type 'tool-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 65
                          :payload '(:tool-call (:id "call-1" :name "read")
                                      :result (:status ok
                                               :content "contents"))))
          (e-chat-render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 160))
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
          (with-current-buffer (e-chat-test--composer buffer)
            (goto-char (point-max))
            (insert "follow-up draft"))
          (e-chat-test--focus-block-containing "Turn took 3min 25sec")
          (let* ((summary (e-chat-test--focused-block))
                 (summary-id (plist-get summary :block-id)))
            (call-interactively #'e-chat-response-navigation-activate)
            (let* ((children (plist-get
                              (gethash summary-id e-chat-transcript--block-registry)
                              :children))
                   (child-kinds
                    (mapcar
                     (lambda (block-id)
                       (plist-get (gethash block-id e-chat-transcript--block-registry)
                                  :kind))
                     children))
                   (summary-index (cl-position summary-id e-chat-transcript--block-order
                                               :test #'equal)))
              (should (equal child-kinds
                             '(activity-thought activity-reasoning
                               activity-tool-batch activity-thought)))
              (should (equal (cl-subseq e-chat-transcript--block-order
                                        (1+ summary-index)
                                        (+ 1 summary-index
                                           (length children)))
                             children))
              (should (equal (e-chat-test--composer-text-for buffer)
                             "follow-up draft"))
              (e-chat-transcript--focus-block (car children))
              (should (equal (call-interactively
                              #'e-chat-response-navigation-copy)
                             "Thought for 1min 3sec"))
              (e-chat-transcript--focus-block (cadr children))
              (should (equal (call-interactively
                              #'e-chat-response-navigation-copy)
                             "planning"))
              (e-chat-transcript--focus-block (nth 2 children))
              (call-interactively #'e-chat-response-navigation-activate)
              (should e-chat-tool-list-mode)
              (should (= (length (plist-get (e-chat-transcript--tool-list-block)
                                            :tool-items))
                         1))
              (e-chat-tool-list-back)
              (e-chat-transcript--focus-block summary-id)
              (call-interactively #'e-chat-response-navigation-activate)
              (dolist (child children)
                (should-not (gethash child e-chat-transcript--block-registry))
                (should-not (member child e-chat-transcript--block-order))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-progress-activity-records-track-round-ownership ()
  "Provider, reasoning, and tool events build semantic round records."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-records")))
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
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:content "planning")))
          (e-chat-render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 63
                          :payload '(:status done)))
          (e-chat-render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 64
                          :payload '(:id "call-1"
                                      :name "read"
                                      :arguments (:path "file"))))
          (e-chat-render-event
           (e-events-make :type 'tool-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 65
                          :payload '(:tool-call (:id "call-1"
                                                :name "read")
                                      :result (:status ok
                                               :content "contents"))))
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
          (let ((display (e-chat-activity-turn-display "turn-1")))
            (should (= (plist-get display :round-count) 2))
            (should (equal (plist-get display :round-statuses) '(done done)))
            (should (= (plist-get display :tool-count) 1))
            (should (string-match-p "planning"
                                    (plist-get display :expanded-text)))
            (should (string-match-p "read"
                                    (plist-get (car (plist-get display :tool-items))
                                               :name)))
            (should (string-match-p "contents"
                                    (plist-get (car (plist-get display :tool-items))
                                               :output)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-replayed-provider-activity-restores-summary ()
  "Replayed provider boundary events restore settled summary plus expansion."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "chat-provider-replay")
          (e-session-append-message
           store "chat-provider-replay"
           '(:role user :content "inspect" :turn-id "turn-1"))
          (e-session-append-activity-event
           store "chat-provider-replay" "turn-1" 'turn-started nil)
          (e-session-append-activity-event
           store "chat-provider-replay" "turn-1" 'provider-request-started
           '(:status started))
          (e-session-append-activity-event
           store "chat-provider-replay" "turn-1" 'provider-request-finished
           '(:status done))
          (e-session-append-activity-event
           store "chat-provider-replay" "turn-1" 'tool-started
           '(:type tool-call :id "call-1" :name "buffer-read"))
          (e-session-append-activity-event
           store "chat-provider-replay" "turn-1" 'tool-finished
           '(:tool-call (:type tool-call :id "call-1" :name "buffer-read")
             :result (:status ok :content "scratch contents")))
          (e-session-append-activity-event
           store "chat-provider-replay" "turn-1" 'turn-finished nil)
          (e-session-append-message
           store "chat-provider-replay"
           '(:role assistant :content "Final answer." :turn-id "turn-1"))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "chat-provider-replay")
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-provider-replay"))
          (with-current-buffer buffer
            (let ((content (buffer-string)))
              (should (string-match-p
                       "Turn took [0-9]+min [0-9]+sec, 1 tool call\\."
                       content))
              (should-not (string-match-p "Thought for" content)))
            (e-chat-test--focus-block-containing "Turn took")
            (call-interactively #'e-chat-response-navigation-activate)
            (let ((content (buffer-string)))
              (should (string-match-p "Thought for [0-9]+min [0-9]+sec"
                                      content))
              (should (string-match-p "1 tool call" content)))
            (let* ((display (e-chat-activity-turn-display "turn-1"))
                   (summary (e-chat-test--focused-block))
                   (summary-id (plist-get summary :block-id))
                   (child-kinds
                    (mapcar
                     (lambda (block-id)
                       (plist-get (gethash block-id e-chat-transcript--block-registry)
                                  :kind))
                     (plist-get (gethash summary-id
                                        e-chat-transcript--block-registry)
                                :children))))
              (should (= (plist-get display :round-count) 1))
              (should (equal (plist-get display :round-statuses) '(done)))
              (should (= (plist-get display :tool-count) 1))
              (should (equal child-kinds
                             '(activity-thought activity-tool-batch))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-sibling-activity-uses-one-stable-observed-record ()
  "One sibling's provider lifecycle rows share an isolated presentation record."
  (let ((buffer (e-chat-test--buffer nil "chat-sibling-activity")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "selected-turn"
                            :created-at 0))
          (let ((selected-status (e-chat-surface-status))
                (event-base
                 '(:session-id "chat-sibling-activity"
                   :turn-id "selected-turn"
                   :board-id "board-1"
                   :subject-participant-id "participant-sibling"
                   :source-turn-id "source-turn-1"
                   :selected-participant-p nil)))
            (dolist (event
                     (list
                      (append event-base
                              '(:type provider-request-started :created-at 1
                                :payload (:status started)))
                      (append event-base
                              '(:type reasoning-delta :created-at 2
                                :payload (:content "sibling planning")))
                      (append event-base
                              '(:type provider-request-finished :created-at 3
                                :payload (:status done)))
                      (append event-base
                              '(:type turn-failed :created-at 4
                                :payload (:error "sibling failed")))))
              (e-chat-render-event event))
            (let* ((observed-id
                    (e-chat-transcript-observed-turn-id "selected-turn" event-base))
                   (observed (e-chat-activity-turn-display observed-id))
                   (selected (e-chat-activity-turn-display "selected-turn")))
              (should (equal observed-id
                             '(:observed-board-turn
                               :board-id "board-1"
                               :subject-participant-id "participant-sibling"
                               :source-turn-id "source-turn-1"
                               :causal-turn-id "selected-turn")))
              (should observed)
              (should (= (plist-get observed :round-count) 1))
              (should (equal (plist-get observed :round-statuses) '(done)))
              (should (plist-get observed :failed))
              (should-not (plist-get selected :settled))
              (should-not (plist-get selected :failed))
              (should (equal (e-chat-surface-status) selected-status))
              (should (equal (e-chat-activity-progress-turn-id)
                             "selected-turn"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))))





(ert-deftest e-chat-test-turn-finished-does-not-read-private-transcript ()
  "Turn finished does not recover output from the private transcript."
  (let ((buffer (e-chat-test--buffer nil "chat-missed-final")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-session-append-message
           (e-harness-sessions e-chat-harness)
           e-chat-session-id
           '(:role assistant
             :content "Recovered final answer."
             :turn-id "turn-1"))
          (e-chat-render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload '(:reason stop)))
          (should-not (string-match-p "Recovered final answer"
                                      (buffer-string)))
          (should-not (plist-get
                       (gethash "turn-1" e-chat-transcript--turn-registry)
                       :final-rendered)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-progress-rerender-preserves-block-view ()
  "Progress redraws keep block view point inside the active block."
  (let ((buffer (e-chat-test--buffer nil "chat-running-block-preserve")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one two three")
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-2"
                          :created-at 20))
          (goto-char (point-min))
          (search-forward "one two")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (let* ((block-id e-chat-transcript--block-view-block-id)
                 (bounds (e-chat-transcript--block-content-bounds
                          (e-chat-transcript--block-view-block)))
                 (target-point (+ (car bounds) 4)))
            (goto-char target-point)
            (e-chat-activity-advance-progress)
            (should e-chat-block-view-mode)
            (should (equal e-chat-transcript--block-view-block-id block-id))
            (should (= (point) target-point))
            (let ((updated-bounds (e-chat-transcript--block-content-bounds
                                   (e-chat-transcript--block-view-block))))
              (should (<= (car updated-bounds) (point)))
              (should (<= (point) (cdr updated-bounds))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-progress-rerender-preserves-running-tool-list ()
  "Progress redraws keep a running activity tool list and selected item."
  (let ((buffer (e-chat-test--buffer nil "chat-running-tool-list-preserve")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (dolist (tool '(("call-1" "read")
                          ("call-2" "write")))
            (e-chat-render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload (list :type 'tool-call
                                           :id (nth 0 tool)
                                           :name (nth 1 tool)))))
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (goto-char (point-min))
          (search-forward "2 tool calls")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (should e-chat-tool-list-mode)
          (call-interactively #'e-chat-tool-list-next)
          (should (= e-chat-transcript--tool-list-index 1))
          (e-chat-activity-advance-progress)
          (e-ui-work-with-batch-drain
              (e-ui-work-drain-batch :buffer (current-buffer)))
          (should e-chat-tool-list-mode)
          (should (= e-chat-transcript--tool-list-index 1))
          (should (string-match-p "write"
                                  (buffer-substring-no-properties
                                   (overlay-start e-chat-transcript--tool-list-overlay)
                                   (overlay-end e-chat-transcript--tool-list-overlay)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-finalizing-tool-activity-removes-dead-blocks ()
  "Completed tool activity does not leave zero-width blocks in navigation."
  (let ((buffer (e-chat-test--buffer nil "chat-finalize-tool-activity")))
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
                          :payload (list :message
                                         (list :role 'user
                                               :content "inspect prompt"))))
          (e-chat-render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload (list :type 'tool-call
                                         :id "call-1"
                                         :name "read")))
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload (list :message
                                         (list :role 'assistant
                                               :content "final answer"))))
          (e-chat-render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12))
          (should (cl-every #'e-chat-transcript--live-block-record e-chat-transcript--block-order))
          (should-not (cl-some
                       (lambda (block-id)
                         (eq (plist-get (gethash block-id e-chat-transcript--block-registry)
                                        :kind)
                             'activity))
                       e-chat-transcript--block-order)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-response-navigation-skips-dead-activity-to-user-block ()
  "Block navigation from a final response can reach and enter the user prompt."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-skip-dead-activity")))
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
                          :payload (list :message
                                         (list :role 'user
                                               :content "copy this prompt"))))
          (e-chat-render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload (list :type 'tool-call
                                         :id "call-1"
                                         :name "read")))
          (e-chat-render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload (list :message
                                         (list :role 'assistant
                                               :content "final answer"))))
          (e-chat-test--focus-block-containing "final answer")
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "k")))
          (should (eq (plist-get (e-chat-test--focused-block) :kind) 'user))
          (call-interactively #'e-chat-response-navigation-activate)
          (should e-chat-block-view-mode)
          (should (equal (e-chat-transcript--block-action-text (e-chat-transcript--block-view-block))
                         "copy this prompt")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-replayed-failed-turn-expands-inline ()
  "Replayed turn-failed activity renders as a compact expandable block."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "chat-failed-replay")
          (e-session-append-message
           store "chat-failed-replay"
           '(:role user :content "too much context" :turn-id "turn-1"))
          (e-session-append-activity-event
           store "chat-failed-replay" "turn-1" 'turn-failed
           '(:error "OpenAI request failed: (error http 400)"
             :details (:type "response.failed"
                       :response
                       (:error
                        (:code "context_length_exceeded"
                         :message
                         "Your input exceeds the context window.")))))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "chat-failed-replay")
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-failed-replay"))
          (with-current-buffer buffer
            (let ((content (buffer-string)))
              (should (string-match-p "too much context" content))
              (should (string-match-p
                       "Turn failed: OpenAI request failed: (error http 400)"
                       content))
              (should-not (string-match-p "context_length_exceeded" content)))
            (e-chat-test--focus-block-containing "Turn failed")
            (e-chat-response-navigation-activate)
            (let ((content (buffer-string))
                  (block (e-chat-test--focused-block)))
              (should e-chat-block-view-mode)
              (should-not e-chat-response-navigation-mode)
              (should (plist-get block :details-visible-p))
              (should-not (get-buffer e-chat-details-buffer-name))
              (should (string-match-p "context_length_exceeded" content)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-buffer-name e-chat-details-buffer-name))))





(ert-deftest e-chat-test-replayed-failed-provider-start-settles-thinking ()
  "Replay settles provider-started plus turn-failed into a failed thought."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "chat-failed-thinking-replay")
          (e-session-append-message
           store "chat-failed-thinking-replay"
           '(:role user :content "fail" :turn-id "turn-1"))
          (e-session-append-activity-event
           store "chat-failed-thinking-replay" "turn-1" 'turn-started nil)
          (e-session-append-activity-event
           store "chat-failed-thinking-replay" "turn-1"
           'provider-request-started
           '(:status started))
          (e-session-append-activity-event
           store "chat-failed-thinking-replay" "turn-1" 'turn-failed
           '(:error "provider failed"))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "chat-failed-thinking-replay")
          (setq buffer (e-chat-open
                        :harness harness
                        :session-id "chat-failed-thinking-replay"))
          (with-current-buffer buffer
            (let ((display (e-chat-activity-turn-display "turn-1")))
              (should (equal (plist-get display :round-statuses) '(failed)))
              (should (string-match-p
                       "Thought failed after [0-9]+min [0-9]+sec"
                       (plist-get display :expanded-text))))
            (let ((content (buffer-string)))
              (should (string-match-p "Turn failed: provider failed" content))
              (should-not (string-match-p "Thinking\\.\\.\\." content)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))





(ert-deftest e-chat-test-tool-activity-uses-bounded-structured-preview ()
  "Chat activity does not force full model text for structured tool results."
  (let ((buffer (e-chat-test--buffer nil "chat-tool-structured-preview")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((e-chat-tool-activity-preview-bytes 32))
            (e-chat-render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"))
            (e-chat-render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:id "call-1" :name "structured")))
            (e-chat-render-event
             (e-events-make :type 'tool-finished
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload
                            (list :tool-call '(:id "call-1" :name "structured")
                                  :result
                                  (list :tool-call-id "call-1"
                                        :name "structured"
                                        :status 'ok
                                        :content (list :items (number-sequence 1 100)
                                                       :body (make-string 200 ?x)))))))
          (let* ((display (e-chat-activity-turn-display "turn-1"))
                 (item (car (plist-get display :tool-items)))
                 (output (plist-get item :output)))
            (should (string-match-p "Tool result preview truncated" output))
            (should-not (string-match-p (make-string 80 ?x) output))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))









(ert-deftest e-chat-test-replayed-run-elisp-action-name-renders ()
  "Replaying a run_elisp tool with a nested action names the action in-buffer."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "chat-run-elisp-replay")
          (e-session-append-message
           store "chat-run-elisp-replay"
           '(:role user :content "run it" :turn-id "turn-1"))
          (e-session-append-activity-event
           store "chat-run-elisp-replay" "turn-1" 'turn-started nil)
          (e-session-append-activity-event
           store "chat-run-elisp-replay" "turn-1" 'provider-request-started
           '(:status started))
          (e-session-append-activity-event
           store "chat-run-elisp-replay" "turn-1" 'tool-started
           '(:type tool-call :id "call-1" :name "run_elisp"))
          (e-session-append-activity-event
           store "chat-run-elisp-replay" "turn-1" 'action-started
           '(:parent-tool-call-id "call-1"
             :capability-id "elisp-job"
             :action :run-batch
             :status started))
          (e-session-append-activity-event
           store "chat-run-elisp-replay" "turn-1" 'tool-finished
           '(:tool-call (:type tool-call :id "call-1" :name "run_elisp")
             :result (:status ok :content "done")))
          (e-session-append-activity-event
           store "chat-run-elisp-replay" "turn-1" 'provider-request-finished
           '(:status done))
          (e-session-append-activity-event
           store "chat-run-elisp-replay" "turn-1" 'turn-finished nil)
          (e-session-append-message
           store "chat-run-elisp-replay"
           '(:role assistant :content "Final answer." :turn-id "turn-1"))
          (e-chat-test--seed-board-log-from-private-fixture
           harness "chat-run-elisp-replay")
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-run-elisp-replay"))
          (with-current-buffer buffer
            (let* ((display (e-chat-activity-turn-display "turn-1"))
                   (item (car (plist-get display :tool-items))))
              (should (string-match-p
                       "run_elisp (elisp-job/run-batch)"
                       (or (plist-get item :name) ""))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))
(provide 'e-chat-transcript-test)

;;; e-chat-transcript-test.el ends here

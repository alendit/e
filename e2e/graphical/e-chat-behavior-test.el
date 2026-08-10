;;; e-chat-behavior-test.el --- Graphical chat behavior tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; User-visible chat contracts that require a real graphical display, command
;; loop, redisplay, timers, or host workspace integration.  These tests must be
;; run through `e2e/run-graphical-tests.sh', never batch Emacs.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-board-runtime)
(require 'e-chat)
(require 'e-chat-session)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-anthropic)
(require 'evil)
(require 'persp-mode)
(setq persp-auto-save-opt 0
      persp-auto-resume-time -1
      persp-save-dir
      (expand-file-name "persp-confs/" user-emacs-directory))
(eval-and-compile
  (let ((directory
         (file-name-directory
          (or load-file-name
              (and (boundp 'byte-compile-current-file)
                   byte-compile-current-file)
              buffer-file-name))))
    (add-to-list 'load-path directory)
    (add-to-list 'load-path (expand-file-name ".." directory))))
(require 'e-board-e2e-support)
(require 'e-graphical-test-support)

(defun e-chat-behavior-test--surface-windows (transcript)
  "Return visible (TRANSCRIPT-WINDOW . COMPOSER-WINDOW) for TRANSCRIPT."
  (let ((transcript-window (get-buffer-window transcript nil))
        (composer-window
         (cl-find-if
          (lambda (window)
            (with-current-buffer (window-buffer window)
              (derived-mode-p 'e-chat-composer-mode)))
          (window-list nil 'nomini))))
    (and (window-live-p transcript-window)
         (window-live-p composer-window)
         (cons transcript-window composer-window))))

(defun e-chat-behavior-test--fixture-windows (fixture)
  "Resolve and retain FIXTURE's current composed-surface windows.
Window objects are presentation state: native redisplay and perspective
restoration may rebuild an equivalent atom and reuse an old window object for
another buffer.  The transcript and composer buffers are the stable fixture
identity, so every settled transition resolves the current pair from them."
  (let* ((transcript (plist-get fixture :transcript))
         (windows (and (buffer-live-p transcript)
                       (e-chat-behavior-test--surface-windows transcript))))
    (should windows)
    (plist-put fixture :transcript-window (car windows))
    (plist-put fixture :composer-window (cdr windows))
    windows))

(defun e-chat-behavior-test--window-mode-line-text (window)
  "Return WINDOW's rendered mode-line text after graphical redisplay."
  (redisplay t)
  (let ((buffer (window-buffer window)))
    (with-current-buffer buffer
      (substring-no-properties
       (format-mode-line mode-line-format nil window buffer)))))

(defun e-chat-behavior-test--open-surface (&optional external-window)
  "Open a public chat surface, optionally beside EXTERNAL-WINDOW.
Return a plist containing its stream, harness, transcript, and visible windows."
  (e-board-e2e-reset-runtime)
  (delete-other-windows)
  (set-frame-size (selected-frame) 140 48)
  (redisplay t)
  (let* ((stream (e-graphical-test-stream-create))
         (harness
          (e-harness-create
           :backend (e-graphical-test-stream-backend stream)
           :default-options
           '(:model "gpt-5.6-sol" :reasoning-effort "high")))
         (session-id
          (e-board-e2e-create-session harness :id "graphical-chat"))
         (transcript (e-chat-open-session harness session-id t)))
    ;; Split the already-composed native atom.  Creating this control window
    ;; before the public open path lets `display-buffer' legitimately reuse it.
    (when external-window
      (let ((outside (get-buffer-create "*e graphical outside*")))
        (set-window-buffer (split-window-right) outside)))
    (e-graphical-test-wait-until
     (lambda ()
       (and (e-chat-behavior-test--surface-windows transcript)
            (with-current-buffer transcript
              (null (e-ui-work-pending (current-buffer))))))
     2.0 "settled composed chat surface")
    (let ((fixture (list :stream stream
                         :harness harness
                         :session-id session-id
                         :transcript transcript)))
      (e-chat-behavior-test--fixture-windows fixture)
      fixture)))

(defun e-chat-behavior-test--cleanup (fixture configuration frame-size)
  "Clean FIXTURE and restore CONFIGURATION and FRAME-SIZE."
  (when-let ((stream (plist-get fixture :stream)))
    (e-graphical-test-stream-cancel stream))
  (when-let ((transcript (plist-get fixture :transcript)))
    (when (buffer-live-p transcript)
      (kill-buffer transcript)))
  (when-let ((outside (get-buffer "*e graphical outside*")))
    (kill-buffer outside))
  (when (window-configuration-p configuration)
    (set-window-configuration configuration))
  (set-frame-size (selected-frame) (car frame-size) (cdr frame-size))
  (redisplay t))

(defun e-chat-behavior-test--submit (fixture prompt)
  "Type and submit PROMPT through FIXTURE's composer command loop."
  (select-window (cdr (e-chat-behavior-test--fixture-windows fixture)))
  (e-graphical-test-type-text prompt)
  (e-graphical-test-send-keys "C-c C-c")
  (e-graphical-test-wait-until
   (lambda ()
     (e-graphical-test-stream-active-p (plist-get fixture :stream)))
   2.0 "backend request after composer submit"))

(defun e-chat-behavior-test--insert-property-rich-history (transcript lines)
  "Insert LINES of protected synthetic history into TRANSCRIPT."
  (with-current-buffer transcript
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (dotimes (index lines)
        (e-chat--insert-protected
         (format "history %04d: rendered transcript property boundary\n" index)
         (if (cl-evenp index)
             'e-chat-assistant-face
           'e-chat-system-face)
         (list 'e-chat-block-id (format "history-%04d" index)
               'e-chat-turn-id (format "history-turn-%04d" (/ index 4)))))
      (e-chat--note-transcript-layout-change))))

(defun e-chat-behavior-test--read-minibuffer-for (seconds before-exit)
  "Enter a real minibuffer for SECONDS, then call BEFORE-EXIT and leave it."
  (let (exit-timer)
    (unwind-protect
        (progn
          (setq exit-timer
                (run-at-time
                 seconds nil
                 (lambda ()
                   (funcall before-exit)
                   (when-let ((window (active-minibuffer-window)))
                     (with-selected-window window
                       (exit-minibuffer))))))
          (minibuffer-with-setup-hook
              (lambda ()
                (when (e-graphical-test-screenshot-enabled-p)
                  (e-graphical-test-capture-state "active-chat-minibuffer"))
                (redisplay t))
            (read-from-minibuffer "Graphical chat line: ")))
      (when (timerp exit-timer)
        (cancel-timer exit-timer)))))

(defun e-chat-behavior-test--emit (fixture item needle)
  "Emit ITEM through FIXTURE and wait until transcript contains NEEDLE."
  (let ((stream (plist-get fixture :stream))
        (transcript (plist-get fixture :transcript)))
    (e-graphical-test-stream-emit stream item)
    (condition-case err
        (e-graphical-test-wait-until
         (lambda ()
           (and (with-current-buffer transcript
                  (string-match-p (regexp-quote needle) (buffer-string)))
                (e-chat-behavior-test--surface-windows transcript)))
         2.0 (format "transcript text %S" needle))
      (error
       (ert-fail
        (with-current-buffer transcript
          (format "%s\nbackend failure: %S\nchat status: %S\nactive turn: %S\nharness events: %S\nboard messages: %S\nattachment current: %S\nsubscription: %S\nruntime activity queue: %S\nruntime deferred hooks: %S\npending UI jobs: %S\ntranscript:\n%s"
                  (error-message-string err)
                  (e-graphical-test-stream-failure stream)
                  e-chat--status
                  (let ((turn
                         (gethash
                          (plist-get fixture :session-id)
                          (e-harness-active-turns
                           (plist-get fixture :harness)))))
                    (list :status (plist-get turn :status)
                          :error (plist-get turn :error)
                          :condition (plist-get turn :condition)))
                  (mapcar
                   (lambda (event) (plist-get event :event-type))
                   (e-harness-session-activity-events
                    (plist-get fixture :harness)
                    (plist-get fixture :session-id)))
                  (let* ((binding
                          (e-chat-service-binding
                           (plist-get fixture :harness)
                           (plist-get fixture :session-id)))
                         (board
                          (e-board-registry-board-source-board
                           (e-chat-service-binding-board binding))))
                    (mapcar
                     (lambda (message)
                       (list (e-board-message-kind message)
                             (e-board-message-activity-kind message)
                             (e-board-message-tags message)
                             (e-board-message-content message)
                             (e-board-message-source-activity-key message)))
                     (e-board-messages board)))
                  (let ((binding
                         (e-chat-service-binding
                          (plist-get fixture :harness)
                          (plist-get fixture :session-id))))
                    (e-board-runtime--current-attachment-p
                     (e-chat-service-binding-attachment binding)))
                  (let ((subscription e-chat--event-subscription))
                    (and subscription
                         (list
                          :active
                          (e-chat-service-subscription-active-p subscription)
                          :state
                          (e-chat-service-subscription-state subscription)
                          :drain-scheduled
                          (e-chat-service-subscription-drain-scheduled
                           subscription))))
                  (and (boundp 'e-board-runtime--pending-activity-head)
                       e-board-runtime--pending-activity-head)
                  (and (boundp 'e-board-runtime--deferred-hook-head)
                       e-board-runtime--deferred-hook-head)
                  (and (boundp 'e-ui-work--pending-jobs)
                       (mapcar
                        (lambda (job)
                          (e-ui-work-spec-id (e-ui-work-job-spec job)))
                        e-ui-work--pending-jobs))
                  (buffer-substring-no-properties
                   (max (point-min) (- (point-max) 2000))
                   (point-max)))))))
    (e-chat-behavior-test--fixture-windows fixture)))

(defun e-chat-behavior-test--finish (fixture answer)
  "Finish FIXTURE's active turn with ANSWER and wait for settlement."
  (e-graphical-test-stream-emit
   (plist-get fixture :stream)
   (list :type 'assistant-message :content answer)
   0.01)
  (e-graphical-test-stream-finish (plist-get fixture :stream) 0.03)
  (e-graphical-test-wait-until
   (lambda ()
     (and (not (e-graphical-test-stream-active-p
                (plist-get fixture :stream)))
          (with-current-buffer (plist-get fixture :transcript)
            (and (string-match-p (regexp-quote answer) (buffer-string))
                 (equal e-chat--status "done")
                 (null (e-ui-work-pending (current-buffer)))
                 (e-chat-behavior-test--surface-windows
                  (current-buffer))))))
   3.0 "settled assistant answer")
  (e-chat-behavior-test--fixture-windows fixture))

(defun e-chat-behavior-test--assert-tail-near-bottom (fixture)
  "Assert that FIXTURE's visible transcript tail is near its window bottom."
  (let ((transcript (plist-get fixture :transcript))
        (window (car (e-chat-behavior-test--fixture-windows fixture))))
    (with-current-buffer transcript
      (redisplay t)
      (let* ((tail (point-max))
             (tail-y (e-graphical-test-tail-y window tail))
             (body-pixels (window-body-height window t))
             (line-pixels (frame-char-height))
             (spacer
              (cl-find-if
               (lambda (overlay)
                 (eq (overlay-get
                      overlay e-chat--output-bottom-spacer-property)
                     window))
               (e-chat--output-bottom-spacer-overlays)))
             (spacer-lines
              (and spacer
                   (length (overlay-get spacer 'before-string)))))
        (ert-info ((format
                    "tail-y=%S body-pixels=%S line-pixels=%S body-lines=%S screen-lines=%S spacer-lines=%S start=%S end=%S point=%S follow=%S output=%S bounds=%S"
                    tail-y body-pixels line-pixels
                    (window-body-height window)
                    (count-screen-lines (point-min) tail nil window)
                    spacer-lines
                    (window-start window) (window-end window t)
                    (window-point window)
                    (e-chat--window-output-follow-state window)
                    (e-chat--output-follow-position)
                    (e-chat--running-status-bounds)))
          (should (pos-visible-in-window-p tail window t))
          (should (integerp tail-y))
          (should (>= tail-y (- body-pixels (* 8 line-pixels)))))))))

(ert-deftest e-chat-behavior-test-zz-debug-screenshots-capture-state-and-transition ()
  "Debug snapshots expose a visual state and a before/after transition pair."
  (skip-unless (display-graphic-p))
  (let ((configuration (current-window-configuration))
        (directory (make-temp-file "e-graphical-screenshots-" t))
        (before-buffer (get-buffer-create "*e screenshot before*"))
        (after-buffer (get-buffer-create "*e screenshot after*")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer before-buffer)
          (insert "visible state before the transition")
          (let* ((state
                  (e-graphical-test-capture-state "initial-state" directory))
                 (transition
                  (e-graphical-test-capture-transition
                   "split-and-select"
                   (lambda ()
                     (let ((window (split-window-right)))
                       (set-window-buffer window after-buffer)
                       (select-window window)
                       (with-current-buffer after-buffer
                         (insert "visible state after the transition"))
                       'transition-complete))
                   directory))
                 (artifacts
                  (list state
                        (plist-get transition :before)
                        (plist-get transition :after))))
            (should (eq (plist-get transition :value) 'transition-complete))
            (e-graphical-test-render-pending-screenshots)
            (dolist (artifact artifacts)
              (should (file-exists-p (plist-get artifact :svg)))
              (should (file-exists-p (plist-get artifact :state)))
              (should (create-image (plist-get artifact :svg) 'svg nil)))
            (with-temp-buffer
              (insert-file-contents
               (plist-get (plist-get transition :after) :svg))
              (should (search-forward "visible state after the transition"
                                      nil t)))
            (with-temp-buffer
              (insert-file-contents
               (plist-get (plist-get transition :after) :state))
              (should (search-forward "*e screenshot after*" nil t)))))
      (set-window-configuration configuration)
      (when (buffer-live-p before-buffer)
        (kill-buffer before-buffer))
      (when (buffer-live-p after-buffer)
        (kill-buffer after-buffer))
      (delete-directory directory t)
      (redisplay t))))

(ert-deftest e-chat-behavior-test-settled-wait-crosses-event-loop ()
  "A settled graphical wait observes pending native/timer transitions."
  (skip-unless (display-graphic-p))
  (let ((ready t)
        fired
        restore-timer)
    (unwind-protect
        (progn
          (run-at-time
           0 nil
           (lambda ()
             (setq fired t
                   ready nil)
             (setq restore-timer
                   (run-at-time 0.01 nil (lambda () (setq ready t))))))
          (e-graphical-test-wait-until
           (lambda () ready) 1.0 "state stable after pending event")
          (should fired)
          (should ready))
      (when (timerp restore-timer)
        (cancel-timer restore-timer)))))

(ert-deftest e-chat-behavior-test-focus-and-atomic-delete ()
  "Opening focuses the composer; C-x 0 closes the complete chat atom."
  (skip-unless (display-graphic-p))
  (let ((configuration (current-window-configuration))
        (frame-size (cons (frame-width) (frame-height)))
        fixture)
    (unwind-protect
        (progn
          (setq fixture (e-chat-behavior-test--open-surface t))
          (let ((transcript-window (plist-get fixture :transcript-window))
                (composer-window (plist-get fixture :composer-window))
                (outside-window
                 (get-buffer-window "*e graphical outside*" nil)))
            (should (eq (selected-window) composer-window))
            (with-current-buffer (window-buffer composer-window)
              (should (derived-mode-p 'e-chat-composer-mode)))
            (should (window-live-p outside-window))
            (e-graphical-test-send-keys "C-x 0")
            (should-not (window-live-p transcript-window))
            (should-not (window-live-p composer-window))
            (should (window-live-p outside-window))
            (should (eq (selected-window) outside-window))))
      (e-chat-behavior-test--cleanup fixture configuration frame-size))))

(ert-deftest e-chat-behavior-test-reload-refreshes-backend-and-keeps-composer ()
  "Reload refreshes a retained chat endpoint without losing composer state."
  (skip-unless (display-graphic-p))
  (let ((configuration (current-window-configuration))
        (frame-size (cons (frame-width) (frame-height)))
        (e-harness-registry--instances (make-hash-table :test 'equal))
        (e-harness-registry--factories (make-hash-table :test 'equal))
        (e-harness-instance--instances (make-hash-table :test 'equal))
        (e-harness-instance--defaults (make-hash-table :test 'equal))
        fixture)
    (unwind-protect
        (progn
          (setq fixture (e-chat-behavior-test--open-surface))
          (let* ((retained (plist-get fixture :harness))
                 (fresh-backend
                  (e-backend-fake-create :name "fresh after reload" :items nil))
                 (fresh
                  (e-harness-create
                   :backend fresh-backend
                   :sessions (e-harness-sessions retained))))
            (e-harness-activate-capability
             fresh (e-chat-session-capability-create))
            (e-harness-registry-register-factory :chat-test (lambda () fresh))
            (let ((e-chat-default-harness-id :chat-test))
              (select-window (plist-get fixture :composer-window))
              (e-graphical-test-type-text "draft across reload")
              (should (= (e-chat-reload-buffers) 1)))
            (e-chat-behavior-test--fixture-windows fixture)
            (should (eq (plist-get fixture :harness) retained))
            (should (eq (e-harness-backend retained) fresh-backend))
            (let ((transcript (plist-get fixture :transcript))
                  (composer
                   (window-buffer (plist-get fixture :composer-window))))
              (should (eq (buffer-local-value
                           'e-chat--surface-transcript-buffer composer)
                          transcript))
              (with-current-buffer transcript
                (setq-local e-chat--mode-line-status
                            "e-chat gpt-5.6-sol/high 18% (64k/353k tok)")
                (setq-local mode-name
                            '(:eval (format-mode-line mode-name))))
              (with-current-buffer composer
                (should
                 (equal (e-chat--surface-composer-mode-name)
                        "e-chat gpt-5.6-sol/high 18%% (64k/353k tok)"))))
            (should (eq (selected-window)
                        (plist-get fixture :composer-window)))
            (with-current-buffer
                (window-buffer (plist-get fixture :composer-window))
              (should (string-suffix-p
                       "draft across reload"
                       (buffer-substring-no-properties
                        (point-min) (point-max)))))))
      (e-chat-behavior-test--cleanup fixture configuration frame-size))))

(ert-deftest e-chat-behavior-test-short-output-stays-bottom-and-keeps-draft ()
  "Short streaming output stays low without stealing composer focus or draft."
  (skip-unless (display-graphic-p))
  (let ((configuration (current-window-configuration))
        (frame-size (cons (frame-width) (frame-height)))
        fixture)
    (unwind-protect
        (progn
          (setq fixture (e-chat-behavior-test--open-surface))
          (e-chat-behavior-test--submit fixture "short prompt")
          (e-chat-behavior-test--emit
           fixture
           '(:type reasoning-delta :stream-kind summary
             :content "first graphical progress")
           "first graphical progress")
          (e-chat-behavior-test--assert-tail-near-bottom fixture)
          (should (eq (selected-window) (plist-get fixture :composer-window)))
          (e-graphical-test-type-text "draft survives")
          (e-chat-behavior-test--emit
           fixture
           '(:type reasoning-delta :stream-kind summary
             :content " second graphical progress")
           "second graphical progress")
          (should (eq (selected-window) (plist-get fixture :composer-window)))
          (with-current-buffer
              (window-buffer (plist-get fixture :composer-window))
            (should (string-suffix-p
                     "draft survives"
                     (buffer-substring-no-properties
                      (point-min) (point-max)))))
          (e-chat-behavior-test--assert-tail-near-bottom fixture)
          (e-chat-behavior-test--finish fixture "short graphical answer"))
      (e-chat-behavior-test--cleanup fixture configuration frame-size))))

(ert-deftest e-chat-behavior-test-reasoning-renders-content-not-payload ()
  "Board-backed reasoning renders prose without its protocol payload."
  (skip-unless (display-graphic-p))
  (let ((configuration (current-window-configuration))
        (frame-size (cons (frame-width) (frame-height)))
        fixture)
    (unwind-protect
        (progn
          (setq fixture (e-chat-behavior-test--open-surface))
          (e-chat-behavior-test--submit fixture "reasoning prompt")
          (e-chat-behavior-test--emit
           fixture
           '(:type reasoning-delta :stream-kind summary
             :content "clean graphical reasoning")
           "clean graphical reasoning")
          (let ((transcript (plist-get fixture :transcript))
                (window (plist-get fixture :transcript-window)))
            (with-current-buffer transcript
              (goto-char (point-min))
              (should (search-forward "clean graphical reasoning" nil t))
              (should (pos-visible-in-window-p (match-beginning 0) window t))
              (should-not (string-match-p "(:type reasoning-delta"
                                          (buffer-string)))
              (should-not (string-match-p ":stream-kind summary"
                                          (buffer-string)))))
          (should (eq (selected-window) (plist-get fixture :composer-window)))
          (e-chat-behavior-test--finish fixture "reasoned graphical answer"))
      (e-chat-behavior-test--cleanup fixture configuration frame-size))))

(ert-deftest e-chat-behavior-test-focused-composer-shows-model-context-fill ()
  "Async catalog fill reaches the focused composer's visible mode line."
  (skip-unless (display-graphic-p))
  (let ((configuration (current-window-configuration))
        (frame-size (cons (frame-width) (frame-height)))
        (e-anthropic--context-window-cache (make-hash-table :test 'equal))
        (e-anthropic--context-window-refresh-requests
         (make-hash-table :test 'equal))
        (e-anthropic--context-window-failure-cache
         (make-hash-table :test 'equal))
        (catalog-calls 0)
        fixture)
    (unwind-protect
        (cl-letf (((symbol-function 'e-anthropic--headers)
                   (lambda (&rest _) nil))
                  ((symbol-function 'e-anthropic--http-get-start)
                   (lambda (&rest args)
                     (cl-incf catalog-calls)
                     (let ((complete (plist-get args :on-complete)))
                       (run-at-time
                        0.05 nil complete
                        "{\"data\":[{\"id\":\"gpt-5.6-sol\",\"max_input_tokens\":353400}]}"))
                     (e-backend-request-create
                      :metadata '(:test graphical-catalog)))))
          (setq fixture (e-chat-behavior-test--open-surface))
          (let ((composer-window (plist-get fixture :composer-window)))
            (e-graphical-test-wait-until
             (lambda ()
               (let ((text
                      (e-chat-behavior-test--window-mode-line-text
                       composer-window)))
                 (and (string-match-p "gpt-5\\.6-sol/high" text)
                      (string-match-p "/353k tok" text))))
             2.0 "focused composer model and context-fill mode line")
            (should (eq (selected-window) composer-window))
            (e-chat-behavior-test--submit fixture "context status prompt")
            (e-graphical-test-stream-emit
             (plist-get fixture :stream)
             '(:type token-usage
               :usage (:input-tokens 64000
                       :cached-input-tokens 0
                       :output-tokens 100
                       :reasoning-output-tokens 0
                       :total-tokens 64100)))
            (e-chat-behavior-test--finish fixture "context status answer")
            (condition-case err
                (e-graphical-test-wait-until
                 (lambda ()
                   (let ((text
                          (e-chat-behavior-test--window-mode-line-text
                           composer-window)))
                     (and (string-match-p "18%" text)
                          (string-match-p "64k/353k tok" text))))
                 2.0 "focused composer provider context-fill update")
              (error
               (ert-fail
                (format
                 "%s\ncomposer mode line: %S\ntranscript mode name: %S\ncomputed status: %S\nlatest usage: %S\npending UI: %S\nactivity: %S"
                 (error-message-string err)
                 (e-chat-behavior-test--window-mode-line-text composer-window)
                 (buffer-local-value
                  'mode-name (plist-get fixture :transcript))
                 (with-current-buffer (plist-get fixture :transcript)
                   (e-chat--mode-line-status-text t))
                 (e-session-latest-token-usage-event
                  (e-harness-sessions (plist-get fixture :harness))
                  (plist-get fixture :session-id))
                 (with-current-buffer (plist-get fixture :transcript)
                   (mapcar
                    (lambda (job)
                      (list (plist-get job :id) (plist-get job :owner)))
                    (e-ui-work-pending (current-buffer))))
                 (mapcar
                  (lambda (event)
                    (list (plist-get event :event-type)
                          (plist-get event :payload)))
                  (e-harness-session-activity-events
                   (plist-get fixture :harness)
                   (plist-get fixture :session-id)))))))
            (should (eq (selected-window) composer-window))))
          (should (= catalog-calls 1))
      (e-chat-behavior-test--cleanup fixture configuration frame-size))))

(ert-deftest e-chat-behavior-test-stream-unpins-and-repins-by-user-scroll ()
  "User scrolling unpins streamed output; reaching the tail repins it."
  (skip-unless (display-graphic-p))
  (let ((configuration (current-window-configuration))
        (frame-size (cons (frame-width) (frame-height)))
        fixture)
    (unwind-protect
        (progn
          (setq fixture (e-chat-behavior-test--open-surface))
          (e-chat-behavior-test--submit fixture "history prompt")
          (e-chat-behavior-test--finish
           fixture
           (mapconcat (lambda (number) (format "history row %03d" number))
                      (number-sequence 1 240) "\n"))
          (e-chat-behavior-test--submit fixture "stream prompt")
          (e-chat-behavior-test--emit
           fixture '(:type reasoning-delta :content "stream update one")
           "stream update one")
          (e-chat-behavior-test--assert-tail-near-bottom fixture)
          (let* ((transcript (plist-get fixture :transcript))
                 (window (plist-get fixture :transcript-window)))
            (select-window (plist-get fixture :composer-window))
            (e-graphical-test-send-keys "C-M-S-v")
            (let ((scrolled-start (window-start window)))
              (with-current-buffer transcript
                (should (< (window-end window t) (point-max))))
              (e-chat-behavior-test--emit
               fixture '(:type reasoning-delta :content " stream update two")
               "stream update two")
              (should (= (window-start window) scrolled-start))
              (should (eq (selected-window)
                          (plist-get fixture :composer-window))))
            (let ((remaining 80))
              (while (and (> remaining 0)
                          (with-current-buffer transcript
                            (< (window-end window t) (point-max))))
                (setq remaining (1- remaining))
                (e-graphical-test-send-keys "C-M-v"))
              (should (> remaining 0)))
            (e-chat-behavior-test--emit
             fixture '(:type reasoning-delta :content " stream update three")
             "stream update three")
            (with-current-buffer transcript
              (should (= (window-point window) (point-max))))
            (e-chat-behavior-test--assert-tail-near-bottom fixture)
            (should (eq (selected-window)
                        (plist-get fixture :composer-window))))
          (e-chat-behavior-test--finish fixture "stream graphical answer"))
      (e-chat-behavior-test--cleanup fixture configuration frame-size))))

(ert-deftest e-chat-behavior-test-large-transcript-minibuffer-keeps-provider-responsive ()
  "A large active chat does not repaint-loop while a minibuffer owns input."
  (skip-unless (display-graphic-p))
  (let ((configuration (current-window-configuration))
        (frame-size (cons (frame-width) (frame-height)))
        (e-chat-progress-interval 0.05)
        fixture)
    (unwind-protect
        (progn
          (setq fixture (e-chat-behavior-test--open-surface))
          (let* ((transcript (plist-get fixture :transcript))
                 (stream (plist-get fixture :stream))
                 (window (car (e-chat-behavior-test--fixture-windows fixture)))
                 tick-before
                 tick-before-exit
                 deferred-before-exit
                 provider-settled-before-exit)
            (e-chat-behavior-test--insert-property-rich-history transcript 500)
            (e-chat-behavior-test--submit fixture "large transcript prompt")
            (select-window window)
            (with-current-buffer transcript
              (goto-char (point-min))
              (forward-line 300)
              (set-window-point window (point))
              (set-window-start window (line-beginning-position) t)
              ;; The submitted turn's first frame is already visible.  Isolate
              ;; the repeating interval from any one-shot redraw queued before
              ;; the recursive minibuffer starts.
              (e-chat--cancel-pending-activity-redraw)
              (setq tick-before (buffer-chars-modified-tick)))
            (e-chat-behavior-test--read-minibuffer-for
             0.25
             (lambda ()
               (setq tick-before-exit
                     (with-current-buffer transcript
                       (buffer-chars-modified-tick))
                     deferred-before-exit
                     (with-current-buffer transcript
                       (copy-tree e-chat--deferred-activity-redraw)))
               (when (e-graphical-test-screenshot-enabled-p)
                 (e-graphical-test-capture-state
                  "large-chat-before-minibuffer-exit"))))
            (should (= tick-before-exit tick-before))
            (should (equal (cdr deferred-before-exit) 'progress))
            (e-graphical-test-wait-until
             (lambda ()
               (with-current-buffer transcript
                 (null e-chat--deferred-activity-redraw)))
             1.0 "deferred activity redraw after minibuffer exit")
            ;; Provider events still cross the production async boundary while
            ;; the next recursive minibuffer owns input.  Only their cosmetic
            ;; repaint is allowed to wait.
            (e-graphical-test-stream-emit
             stream '(:type assistant-message :content "responsive answer") 0.03)
            (e-graphical-test-stream-finish stream 0.06)
            (e-chat-behavior-test--read-minibuffer-for
             0.25
             (lambda ()
               (setq provider-settled-before-exit
                     (and (not (e-graphical-test-stream-active-p stream))
                          (with-current-buffer transcript
                            (string-match-p "responsive answer"
                                            (buffer-string)))))))
            (should provider-settled-before-exit)
            (e-graphical-test-wait-until
             (lambda ()
               (with-current-buffer transcript
                 (and (equal e-chat--status "done")
                      (null (e-ui-work-pending (current-buffer))))))
             3.0 "large chat settled after minibuffer provider completion")))
      (e-chat-behavior-test--cleanup fixture configuration frame-size))))

(ert-deftest e-chat-behavior-test-persp-switch-restores-focused-surface ()
  "A real persp-mode round trip restores the chat atom and composer focus."
  (skip-unless (display-graphic-p))
  (let ((configuration (current-window-configuration))
        (frame-size (cons (frame-width) (frame-height)))
        fixture)
    (unwind-protect
        (progn
          (persp-mode 1)
          (persp-switch "e-graphical-chat")
          (setq fixture (e-chat-behavior-test--open-surface))
          (let ((transcript (plist-get fixture :transcript)))
            (persp-switch "e-graphical-away")
            (switch-to-buffer (get-buffer-create "*e graphical away*"))
            (persp-switch "e-graphical-chat")
            (e-graphical-test-wait-until
             (lambda ()
               (let ((windows
                      (e-chat-behavior-test--surface-windows transcript)))
                 (and windows
                      (eq (selected-window) (cdr windows)))))
             3.0 "persp-restored chat surface with focused composer")))
      (when (bound-and-true-p persp-mode)
        (persp-mode -1))
      (when-let ((away (get-buffer "*e graphical away*")))
        (kill-buffer away))
      (e-chat-behavior-test--cleanup fixture configuration frame-size)
      ;; `persp-mode' finishes disabling through the interactive event loop.
      ;; Let those callbacks settle, then make the captured pre-test window
      ;; configuration the final state so later graphical cases are isolated.
      (sit-for 0.05)
      (set-window-configuration configuration)
      (redisplay t))))

(provide 'e-chat-behavior-test)

;;; e-chat-behavior-test.el ends here

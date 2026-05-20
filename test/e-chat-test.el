;;; e-chat-test.el --- Tests for e chat presentation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the chat buffer presentation shell.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-chat)
(require 'e-events)
(require 'e-harness)

(defun e-chat-test--buffer (&optional items session-id)
  "Return a chat buffer backed by fake backend ITEMS and SESSION-ID."
  (let* ((backend (e-backend-fake-create :items items))
         (harness (e-harness-create :backend backend)))
    (e-chat-open :harness harness
                 :session-id (or session-id "chat-test"))))

(defun e-chat-test--kill-chat-buffers ()
  "Kill all live e chat buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'e-chat-mode)
          (kill-buffer buffer))))))

(defun e-chat-test--render-turn (turn-id start-time end-time prompt response)
  "Render TURN-ID with START-TIME, END-TIME, PROMPT, and RESPONSE."
  (e-chat--render-event
   (e-events-make :type 'turn-started
                  :session-id e-chat-session-id
                  :turn-id turn-id
                  :created-at start-time))
  (e-chat--render-event
   (e-events-make :type 'message-added
                  :session-id e-chat-session-id
                  :turn-id turn-id
                  :created-at start-time
                  :payload (list :message
                                 (list :role 'user :content prompt))))
  (e-chat--render-event
   (e-events-make :type 'message-added
                  :session-id e-chat-session-id
                  :turn-id turn-id
                  :created-at end-time
                  :payload (list :message
                                 (list :role 'assistant :content response))))
  (e-chat--render-event
   (e-events-make :type 'turn-finished
                  :session-id e-chat-session-id
                  :turn-id turn-id
                  :created-at end-time
                  :payload '(:reason stop))))

(defun e-chat-test--focused-turn-bounds ()
  "Return focused turn overlay bounds."
  (list (overlay-start e-chat--focused-turn-overlay)
        (overlay-end e-chat--focused-turn-overlay)))

(defun e-chat-test--focused-turn-text ()
  "Return focused overlay text."
  (buffer-substring-no-properties
   (overlay-start e-chat--focused-turn-overlay)
   (overlay-end e-chat--focused-turn-overlay)))

(defun e-chat-test--turn-bounds (turn-id)
  "Return registered bounds for TURN-ID."
  (let ((turn (gethash turn-id e-chat--turn-registry)))
    (list (marker-position (plist-get turn :start-marker))
          (marker-position (plist-get turn :end-marker)))))

(ert-deftest e-chat-test-open-creates-protected-transcript-and-composer ()
  "Opening chat creates protected transcript text and editable composer text."
  (let ((buffer (e-chat-test--buffer nil "chat-open")))
    (unwind-protect
        (with-current-buffer buffer
          (should (derived-mode-p 'e-chat-mode))
          (goto-char (point-min))
          (should (looking-at-p (regexp-quote e-chat--title)))
          (should (eq (get-text-property (point-min) 'font-lock-face)
                      'e-chat-title-face))
          (should-not (string-match-p "^e chat$" (buffer-string)))
          (should (markerp e-chat--composer-start-marker))
          (should (marker-position e-chat--composer-start-marker))
          (should (get-text-property (point-min) 'read-only))
          (goto-char (point-min))
          (should-error (insert "mutate") :type 'text-read-only)
          (goto-char (point-max))
          (insert "editable")
          (should (equal (e-chat--composer-text) "editable"))
          (should (string-match-p (regexp-quote e-chat--composer-glyph)
                                  (buffer-string)))
          (should (get-text-property
                   (1- (marker-position e-chat--composer-start-marker))
                   'e-chat-composer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-has-visible-separator-line ()
  "The composer has a protected, visible separator above its prompt."
  (let ((buffer (e-chat-test--buffer nil "chat-separator")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char e-chat--composer-start-marker)
          (forward-line -1)
          (let ((line-start (line-beginning-position))
                (line-end (line-end-position)))
            (should (string-match-p "─"
                                    (buffer-substring-no-properties
                                     line-start line-end)))
            (should (eq (get-text-property line-start 'font-lock-face)
                        'e-chat-separator-face))
            (should (get-text-property line-start 'read-only))
            (should (equal (face-attribute 'e-chat-separator-face :foreground)
                           "#7f8a99"))
            (should (equal (face-attribute 'e-chat-separator-face :background)
                           "#202833"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-submit-multiline-composer-and-render-response ()
  "The composer submits multiline text and chat renders message blocks."
  (let ((buffer (e-chat-test--buffer
                 '((:type assistant-message :content "hello back")
                   (:type done :reason stop))
                 "chat-submit")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "first line\nsecond line")
          (e-chat-submit)
          (e-harness-wait e-chat-harness e-chat-session-id 1.0)
          (let ((content (buffer-string)))
            (should (string-match-p (concat (regexp-quote e-chat--user-glyph)
                                            " first line\nsecond line")
                                    content))
            (should (string-match-p
                     (concat (regexp-quote e-chat--assistant-glyph)
                             " hello back")
                     content))
            (should-not (string-match-p "Turn started" content))
            (should-not (string-match-p "Turn finished" content))
            (should-not (string-match-p "Backend returned no assistant output"
                                        content)))
          (save-excursion
            (goto-char (point-min))
            (search-forward "first line")
            (should (eq (get-text-property (point) 'font-lock-face)
                        'e-chat-user-face))
            (search-forward "hello back")
            (should (eq (get-text-property (point) 'font-lock-face)
                        'e-chat-assistant-face)))
          (should (equal (e-chat--composer-text) "")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-submit-immediately-clears-composer-and-renders-user-turn ()
  "Submitting shows the user turn before the async backend settles."
  (let ((buffer (e-chat-test--buffer
                 '((:type assistant-message :content "later")
                   (:type done :reason stop))
                 "chat-submit-immediate")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "send now")
          (e-chat-submit)
          (let ((content (buffer-string)))
            (should (string-match-p
                     (concat (regexp-quote e-chat--user-glyph)
                             " send now")
                     content))
            (should-not (string-match-p
                         (concat (regexp-quote e-chat--composer-glyph)
                                 "send now")
                         content)))
          (should-not (e-chat--composer-active-p)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-submit-forces-redisplay-after-human-turn ()
  "Submitting forces the rendered human turn to display before timers run."
  (let ((buffer (e-chat-test--buffer
                 '((:type assistant-message :content "later")
                   (:type done :reason stop))
                 "chat-submit-redisplay"))
        redisplayed)
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "send now")
          (cl-letf (((symbol-function 'redisplay)
                     (lambda (&optional force)
                       (setq redisplayed force))))
            (e-chat-submit))
          (should (equal redisplayed t))
          (should (string-match-p
                   (concat (regexp-quote e-chat--user-glyph)
                           " send now")
                   (buffer-string)))
          (should-not (e-chat--composer-active-p)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-return-inserts-newline-in-composer ()
  "RET inserts a newline instead of submitting the prompt."
  (let ((buffer (e-chat-test--buffer
                 '((:type assistant-message :content "unexpected")
                   (:type done :reason stop))
                 "chat-ret")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "one")
          (call-interactively (lookup-key e-chat-mode-map (kbd "RET")))
          (insert "two")
          (should (equal (e-chat--composer-text) "one\ntwo"))
          (should (equal (e-harness-messages e-chat-harness e-chat-session-id)
                         nil)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-no-evil-setup-is-required ()
  "Opening chat does not configure or force Evil modal state."
  (let (evil-configured evil-insert-called evil-local-mode-argument buffer)
    (cl-letf (((symbol-function 'evil-set-initial-state)
               (lambda (&rest _args)
                 (setq evil-configured t)))
              ((symbol-function 'evil-insert-state)
               (lambda ()
                 (setq evil-insert-called t)))
              ((symbol-function 'evil-local-mode)
               (lambda (argument)
                 (setq evil-local-mode-argument argument))))
      (unwind-protect
          (progn
            (setq buffer (e-chat-test--buffer nil "chat-no-evil"))
            (should-not evil-configured)
            (should-not evil-insert-called)
            (should (equal evil-local-mode-argument -1)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest e-chat-test-composer-disables-completion ()
  "The composer does not trigger completion UI or completion-at-point."
  (let (company-mode-argument corfu-mode-argument buffer)
    (cl-letf (((symbol-function 'company-mode)
               (lambda (argument)
                 (setq company-mode-argument argument)))
              ((symbol-function 'corfu-mode)
               (lambda (argument)
                 (setq corfu-mode-argument argument)
                 (kill-local-variable 'completion-in-region-function))))
      (unwind-protect
          (progn
            (setq buffer (e-chat-test--buffer nil "chat-no-completion"))
            (with-current-buffer buffer
              (should (local-variable-p 'completion-at-point-functions))
              (should-not completion-at-point-functions)
              (should (local-variable-p 'completion-in-region-function))
              (should (eq completion-in-region-function #'ignore))
              (should (equal company-mode-argument -1))
              (should (equal corfu-mode-argument -1))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest e-chat-test-speaker-faces-are-visually-distinct ()
  "User, assistant, and system entries use distinct background highlighting."
  (should (equal (face-attribute 'e-chat-user-face :background)
                 "#243347"))
  (should (equal (face-attribute 'e-chat-assistant-face :background)
                 "#2b3526"))
  (should (equal (face-attribute 'e-chat-system-face :background)
                 "#312b3c"))
  (should-not (equal (face-attribute 'e-chat-user-face :background)
                     (face-attribute 'e-chat-assistant-face :background)))
  (should (eq (face-attribute 'e-chat-user-face :extend) t))
  (should (eq (face-attribute 'e-chat-assistant-face :extend) t))
  (should (eq (face-attribute 'e-chat-system-face :extend) t)))

(ert-deftest e-chat-test-owned-face-defaults-refresh-separator-face ()
  "Live reload reapplies package-owned separator face defaults."
  (unwind-protect
      (progn
        (set-face-attribute 'e-chat-separator-face nil
                            :foreground 'unspecified
                            :background 'unspecified)
        (e-chat--apply-owned-face-defaults)
        (should (equal (face-attribute 'e-chat-separator-face :foreground)
                       "#7f8a99"))
        (should (equal (face-attribute 'e-chat-separator-face :background)
                       "#202833")))
    (e-chat--apply-owned-face-defaults)))

(ert-deftest e-chat-test-user-and-assistant-headings-are-glyph-only ()
  "User and assistant message headings render only their compact glyphs."
  (should (equal e-chat--user-glyph ">"))
  (should (equal e-chat--assistant-glyph "●")))

(ert-deftest e-chat-test-hides-empty-output-diagnostic ()
  "The chat buffer does not render transient empty-output diagnostics."
  (let ((buffer (e-chat-test--buffer
                 '((:type done :reason stop))
                 "chat-empty-output")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-submit "hello")
          (e-harness-wait e-chat-harness e-chat-session-id 1.0)
          (should-not (string-match-p "Backend returned no assistant output"
                                      (buffer-string)))
          (should (string-match-p
                   (concat (regexp-quote e-chat--assistant-glyph)
                           " ✅ Done")
                   (buffer-string)))
          (should (string-match-p "E Chat: done" header-line-format)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-hides-transient-tool-entries ()
  "Tool progress stays out of the transcript after the turn settles."
  (let ((buffer (e-chat-test--buffer nil "chat-events")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-failed
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:error "boom")))
          (e-chat--render-event
           (e-events-make :type 'turn-cancelled
                          :session-id e-chat-session-id
                          :turn-id "turn-2"))
          (e-chat--render-event
           (e-events-make :type 'tool-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-3"
                          :payload '(:result (:status ok))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     (concat (regexp-quote e-chat--system-glyph)
                             " System\nTurn failed: boom")
                     content))
            (should (string-match-p
                     (concat (regexp-quote e-chat--system-glyph)
                             " System\nTurn cancelled")
                     content))
            (should-not (string-match-p "(:status ok)" content))))
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
          (should (equal (plist-get (gethash e-chat--focused-block-id
                                             e-chat--block-registry)
                                    :turn-id)
                         "turn-2"))
          (should (string-match-p
                   (concat (regexp-quote e-chat--assistant-glyph) " two")
                   (e-chat-test--focused-turn-text)))
          (should-not (string-match-p
                       (concat (regexp-quote e-chat--user-glyph) " second")
                       (e-chat-test--focused-turn-text))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-focus-excludes-composer-spacer ()
  "Focused turn highlight stops before the visible composer spacer."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-spacer"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (let ((e-chat--test-window-body-height 60)
                  (e-chat--test-transcript-screen-lines 4))
              (e-chat-test--render-turn "turn-1" 10 11 "first" "one"))
            (goto-char (point-max))
            (call-interactively #'e-chat-enter-response-navigation)
            (should (equal (plist-get (gethash e-chat--focused-block-id
                                               e-chat--block-registry)
                                      :turn-id)
                           "turn-1"))
            (should (<= (overlay-end e-chat--focused-turn-overlay)
                        (marker-position e-chat--transcript-end-marker)))))
      (when (window-live-p window)
        (delete-window window))
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
          (should (equal (plist-get (gethash e-chat--focused-block-id
                                             e-chat--block-registry)
                                    :turn-id)
                         "turn-1"))
          (should (string-match-p
                   (concat (regexp-quote e-chat--assistant-glyph) " one")
                   (e-chat-test--focused-turn-text)))
          (should-not (string-match-p
                       (concat (regexp-quote e-chat--user-glyph) " first")
                       (e-chat-test--focused-turn-text))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-j-and-k-move-focus ()
  "Response navigation j/k move between turn blocks."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-move")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 23.5 "second" "two")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "k")))
          (should (string-match-p
                   (concat (regexp-quote e-chat--user-glyph) " second")
                   (e-chat-test--focused-turn-text)))
          (should-not (string-match-p
                       (concat (regexp-quote e-chat--assistant-glyph) " two")
                       (e-chat-test--focused-turn-text)))
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "j")))
          (should (string-match-p
                   (concat (regexp-quote e-chat--assistant-glyph) " two")
                   (e-chat-test--focused-turn-text))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-expand-inserts-readable-turn-details ()
  "Expanding a focused turn inserts readable metadata under that turn."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-expand")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 23.5 "second" "two")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-expand)
          (let ((content (buffer-string)))
            (should (string-match-p
                     (concat (regexp-quote e-chat--assistant-glyph)
                             " two\n\n  Turn: turn-2")
                     content))
            (should (string-match-p
                     "  Started: 1970-01-01 00:00:20 UTC"
                     content))
            (should (string-match-p
                     "  Ended: 1970-01-01 00:00:23 UTC"
                     content))
            (should (string-match-p "  Duration: 3.50s" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-expand-folds-visible-turn-details ()
  "Pressing RET on an unfolded focused turn removes its inline metadata."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-fold")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-expand)
          (should (string-match-p "  Turn: turn-1" (buffer-string)))
          (call-interactively #'e-chat-response-navigation-expand)
          (should-not (string-match-p "  Turn: turn-1" (buffer-string)))
          (should e-chat-response-navigation-mode)
          (should (equal e-chat--focused-turn-id "turn-1")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-i-returns-to-composer ()
  "Pressing i leaves navigation mode and focuses the composer."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-insert")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively
           (lookup-key e-chat-response-navigation-mode-map (kbd "i")))
          (should-not e-chat-response-navigation-mode)
          (should (>= (point) (marker-position e-chat--composer-start-marker))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-replayed-session-uses-synthetic-turns ()
  "Replayed messages without turn metadata remain navigable."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "chat-nav-replay")
          (e-session-append-message
           store "chat-nav-replay" '(:role user :content "old first"))
          (e-session-append-message
           store "chat-nav-replay" '(:role assistant :content "old one"))
          (e-session-append-message
           store "chat-nav-replay" '(:role user :content "old second"))
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-nav-replay"))
          (with-current-buffer buffer
            (call-interactively #'e-chat-enter-response-navigation)
            (should (equal e-chat--focused-turn-id "replayed-turn-2"))
            (call-interactively #'e-chat-response-navigation-expand)
            (let ((content (buffer-string)))
              (should (string-match-p "  Started: unknown" content))
              (should (string-match-p "  Ended: unknown" content))
              (should (string-match-p "  Duration: unknown" content)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-uses-bottom-spacer-when-buffer-is-visible ()
  "Displayed chat buffers keep the composer visually near the window bottom."
  (let ((buffer (e-chat-test--buffer nil "chat-bottom"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (let ((e-chat--test-window-body-height 12))
              (e-chat--refresh-composer-position))
            (should (markerp e-chat--composer-spacer-marker))
            (should (marker-position e-chat--composer-spacer-marker))
            (should (< (marker-position e-chat--composer-spacer-marker)
                       (marker-position e-chat--composer-start-marker)))
            (should (string-match-p "\n\n\n"
                                    (buffer-substring-no-properties
                                     e-chat--composer-spacer-marker
                                     e-chat--composer-start-marker)))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-skips-spacer-when-wrapped-content-fills-window ()
  "Wrapped long transcript content does not create a middle-of-buffer spacer."
  (let ((buffer (e-chat-test--buffer nil "chat-long"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (e-chat--insert-entry
             "Assistant"
             (make-string 400 ?x)
             t)
            (let ((e-chat--test-window-body-height 12)
                  (e-chat--test-transcript-screen-lines 20))
              (e-chat--refresh-composer-position))
            (should (not (string-match-p
                          "\n\n\n"
                          (buffer-substring-no-properties
                           e-chat--composer-spacer-marker
                           e-chat--transcript-end-marker))))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-reload-buffers-refreshes-existing-chat-harness ()
  "Reloading chat buffers reattaches them and restores persisted transcript."
  (let* ((directory (make-temp-file "e-chat-" t))
         (store (e-session-persistent-store-create directory))
         (old-backend (e-backend-fake-create :items nil))
         (old-harness (e-harness-create :backend old-backend :sessions store))
         (new-backend (e-backend-fake-create
                       :items '((:type assistant-message :content "fresh answer")
                                (:type done :reason stop))))
         (new-harness (e-harness-create :backend new-backend :sessions store))
         (buffer (e-chat-open :harness old-harness :session-id "chat-reload")))
    (unwind-protect
        (progn
          (e-session-append-message
           store "chat-reload" '(:id "msg-1" :role user :content "saved prompt"))
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "stale prompt"))
          (cl-letf (((symbol-function 'e-chat--default-harness)
                     (lambda () new-harness)))
            (should (= (e-chat-reload-buffers) 1)))
          (with-current-buffer buffer
            (should (eq e-chat-harness new-harness))
            (should (equal e-chat-session-id "chat-reload"))
            (should-not (string-match-p "stale prompt" (buffer-string)))
            (should (string-match-p "saved prompt" (buffer-string)))
            (e-chat-submit "hello")
            (e-harness-wait e-chat-harness e-chat-session-id 1.0)
            (should (string-match-p
                     (concat (regexp-quote e-chat--assistant-glyph)
                             " fresh answer")
                     (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest e-chat-test-default-harness-uses-default-openai-provider ()
  "Default chat harness creation honors `e-openai-default-provider'."
  (let ((e-openai-default-provider 'openai-compatible-gateway)
        (seen-provider nil)
        (seen-sessions nil))
    (cl-letf (((symbol-function 'e-openai-create-harness)
               (lambda (&rest args)
                 (setq seen-provider (plist-get args :provider))
                 (setq seen-sessions (plist-get args :sessions))
                 (e-harness-create
                  :backend (e-backend-fake-create :items nil)
                  :sessions seen-sessions))))
      (should (e-harness-p (e-chat--default-harness)))
      (should (eq seen-provider 'openai-compatible-gateway))
      (should (e-session-store-p seen-sessions)))))

(ert-deftest e-chat-test-new-creates-distinct-persisted-sessions ()
  "Each new chat command invocation creates a distinct persisted session."
  (let* ((directory (make-temp-file "e-chat-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         first-id second-id)
    (unwind-protect
        (cl-letf (((symbol-function 'e-chat--default-harness)
                   (lambda () harness)))
          (e-chat-test--kill-chat-buffers)
          (with-current-buffer (e-chat-new)
            (setq first-id e-chat-session-id))
          (with-current-buffer (e-chat-new)
            (setq second-id e-chat-session-id))
          (should (not (equal first-id second-id)))
          (should (file-exists-p
                   (expand-file-name (concat first-id ".jsonl")
                                     (expand-file-name "sessions" directory))))
          (should (file-exists-p
                   (expand-file-name (concat second-id ".jsonl")
                                     (expand-file-name "sessions" directory)))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))

(ert-deftest e-chat-test-resume-selects-existing-session ()
  "Resuming uses completing-read over persisted sessions and renders transcript."
  (let* ((directory (make-temp-file "e-chat-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store)))
    (unwind-protect
        (progn
          (e-session-create store :id "resume-me")
          (e-session-append-message
           store "resume-me" '(:id "msg-1" :role user :content "saved hello"))
          (cl-letf (((symbol-function 'e-chat--default-harness)
                     (lambda () harness))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _args)
                       (car collection))))
            (with-current-buffer (e-chat-resume)
              (should (equal e-chat-session-id "resume-me"))
              (should (string-match-p "saved hello" (buffer-string))))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))

(ert-deftest e-chat-test-rename-updates-session-and-buffer-display ()
  "Renaming updates persistent metadata and the attached buffer name."
  (let* ((directory (make-temp-file "e-chat-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer (e-chat-open :harness harness :session-id "rename-me")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _args) "Renamed session")))
            (call-interactively #'e-chat-rename))
          (should (equal (e-session-display-title store "rename-me")
                         "Renamed session"))
          (should (string-match-p "Renamed session" (buffer-name)))
          (should (string-match-p "Renamed session" header-line-format)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest e-chat-test-model-and-effort-commands-update-session-options ()
  "Chat model and effort commands update harness-owned session options."
  (let ((buffer (e-chat-test--buffer nil "chat-options")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _args) "gpt-test"))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _args) "high")))
            (call-interactively #'e-chat-set-model)
            (call-interactively #'e-chat-set-effort))
          (should (equal (e-harness-session-options
                          e-chat-harness
                          e-chat-session-id)
                         '(:model "gpt-test" :reasoning-effort "high")))
          (should (string-match-p "gpt-test" header-line-format))
          (should (string-match-p "high" header-line-format)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-show-context-opens-read-only-preview-buffer ()
  "Context command renders the current session context in a read-only buffer."
  (let ((buffer (e-chat-test--buffer nil "chat-context")))
    (unwind-protect
        (with-current-buffer buffer
          (e-session-append-message
           (e-harness-sessions e-chat-harness)
           e-chat-session-id
           '(:role user :content "context question"))
          (let ((context-buffer (e-chat-show-context)))
            (should (buffer-live-p context-buffer))
            (with-current-buffer context-buffer
              (should (derived-mode-p 'special-mode))
              (should buffer-read-only)
              (should (string-match-p "Session: chat-context" (buffer-string)))
              (should (string-match-p "context question" (buffer-string)))
              (should-error (insert "mutate") :type 'buffer-read-only))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when-let ((context-buffer (get-buffer e-chat-context-buffer-name)))
        (kill-buffer context-buffer)))))

(ert-deftest e-chat-test-reset-clears-rendered-session ()
  "Reset clears the rendered chat buffer and harness session transcript."
  (let ((buffer (e-chat-test--buffer
                 '((:type assistant-message :content "answer")
                   (:type done :reason stop))
                 "chat-reset")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-submit "question")
          (e-harness-wait e-chat-harness e-chat-session-id 1.0)
          (e-chat-reset)
          (should (string-match-p
                   (concat (regexp-quote e-chat--system-glyph)
                           " System\nSession reset")
                   (buffer-string)))
          (should (equal (e-harness-messages e-chat-harness e-chat-session-id)
                         nil))
          (goto-char (point-max))
          (insert "next")
          (should (equal (e-chat--composer-text) "next")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-chat-test)

;;; e-chat-test.el ends here

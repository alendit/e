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
(require 'e-chat-session)
(require 'e-events)
(require 'e-harness)
(require 'e-harness-registry)
(require 'e-tools)

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

(defmacro e-chat-test--with-empty-harness-registry (&rest body)
  "Run BODY with an isolated harness registry."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal)))
     ,@body))

(defun e-chat-test--activate-chat-session (harness)
  "Activate the chat-session capability in HARNESS."
  (e-harness-activate-capability
   harness
   (e-chat-session-capability-create))
  harness)

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

(defun e-chat-test--focused-block ()
  "Return the currently focused block record."
  (gethash e-chat--focused-block-id e-chat--block-registry))

(defun e-chat-test--focus-block-containing (text)
  "Enter response navigation at rendered block containing TEXT."
  (goto-char (point-min))
  (search-forward text)
  (call-interactively #'e-chat-enter-response-navigation)
  (e-chat-test--focused-block))

(defun e-chat-test--kill-buffer-name (name)
  "Kill buffer NAME when it exists."
  (when-let ((buffer (get-buffer name)))
    (kill-buffer buffer)))

(defun e-chat-test--turn-bounds (turn-id)
  "Return registered bounds for TURN-ID."
  (let ((turn (gethash turn-id e-chat--turn-registry)))
    (list (marker-position (plist-get turn :start-marker))
          (marker-position (plist-get turn :end-marker)))))

(defun e-chat-test--count-occurrences (needle text)
  "Return the number of non-overlapping NEEDLE occurrences in TEXT."
  (let ((count 0)
        (start 0))
    (while (and (not (string-empty-p needle))
                (string-match (regexp-quote needle) text start))
      (setq start (match-end 0))
      (setq count (1+ count)))
    count))

(defun e-chat-test--count-font-lock-face-runs (face &optional start end)
  "Return the number of contiguous `font-lock-face' runs using FACE."
  (let ((count 0)
        (limit (or end (point-max)))
        (pos (or start (point-min)))
        next)
    (while (< pos limit)
      (setq next (or (next-single-property-change pos 'font-lock-face
                                                  nil limit)
                     limit))
      (when (eq (get-text-property pos 'font-lock-face) face)
        (setq count (1+ count)))
      (setq pos next))
    count))

(defun e-chat-test--session-subscriber-count (harness session-id)
  "Return HARNESS subscriber count for SESSION-ID."
  (length
   (cl-remove-if-not
    (lambda (subscriber)
      (equal (plist-get subscriber :session-id) session-id))
    (e-harness-subscribers harness))))

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

(ert-deftest e-chat-test-turns-and-responses-have-stable-separators ()
  "Rendered turns use explicit separator text outside navigable blocks."
  (let ((buffer (e-chat-test--buffer nil "chat-turn-separators")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 21 "second" "two")
          (should (boundp 'e-chat--turn-separator))
          (should (boundp 'e-chat--response-separator))
          (let ((content (buffer-string)))
            (should (= (e-chat-test--count-occurrences
                        e-chat--turn-separator content)
                       1))
            (should (= (e-chat-test--count-font-lock-face-runs
                        'e-chat-separator-face
                        (point-min)
                        (marker-position e-chat--composer-spacer-marker))
                       2))
            (should (equal e-chat--response-separator
                           e-chat--composer-separator))
            (should (string-match-p
                     (concat "one\\(.\\|\n\\)*"
                             (regexp-quote e-chat--turn-separator)
                             "\\(.\\|\n\\)*"
                             (regexp-quote e-chat--user-glyph)
                             " second")
                     content)))
          (goto-char (point-min))
          (search-forward e-chat--turn-separator)
          (should (eq (get-text-property (line-beginning-position)
                                         'font-lock-face)
                      'e-chat-turn-separator-face))
          (should (get-text-property (line-beginning-position) 'read-only))
          (should-not (get-text-property (line-beginning-position)
                                         'e-chat-block-id))
          (goto-char (point-min))
          (search-forward e-chat--response-separator)
          (should (eq (get-text-property (line-beginning-position)
                                         'font-lock-face)
                      'e-chat-separator-face))
          (should (get-text-property (line-beginning-position) 'read-only))
          (should-not (get-text-property (line-beginning-position)
                                         'e-chat-block-id)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-running-activity-keeps-single-response-separator ()
  "A running turn keeps one user/assistant separator after final output."
  (let ((buffer (e-chat-test--buffer nil "chat-running-response-separator")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:message (:role user
                                                :content "inspect"))))
          (e-chat--render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type tool-call
                                      :id "call-1"
                                      :name "read")))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "done"))))
          (should (boundp 'e-chat--response-separator))
          (should (= (e-chat-test--count-font-lock-face-runs
                      'e-chat-separator-face
                      (point-min)
                      (or (and (markerp e-chat--composer-spacer-marker)
                               (marker-position e-chat--composer-spacer-marker))
                          (point-max)))
                     1))
          (should-not (string-match-p "2 tool calls" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-navigation-excludes-separators-and-does-not-reflow ()
  "Block navigation changes focus without adding/removing separator text."
  (let ((buffer (e-chat-test--buffer nil "chat-navigation-separator-stability")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 21 "second" "two")
          (should (boundp 'e-chat--turn-separator))
          (should (boundp 'e-chat--response-separator))
          (let ((content-before (buffer-string))
                (lines-before (count-lines (point-min) (point-max))))
            (goto-char (point-min))
            (search-forward "two")
            (call-interactively #'e-chat-enter-response-navigation)
            (should-not (string-match-p
                         (regexp-quote e-chat--turn-separator)
                         (e-chat-test--focused-turn-text)))
            (should-not (string-match-p
                         (regexp-quote e-chat--response-separator)
                         (e-chat-test--focused-turn-text)))
            (call-interactively
             (lookup-key e-chat-response-navigation-mode-map (kbd "k")))
            (should-not (string-match-p
                         (regexp-quote e-chat--turn-separator)
                         (e-chat-test--focused-turn-text)))
            (should-not (string-match-p
                         (regexp-quote e-chat--response-separator)
                         (e-chat-test--focused-turn-text)))
            (should (equal (buffer-string) content-before))
            (should (= (count-lines (point-min) (point-max))
                       lines-before))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-separator-faces-refresh-owned-defaults ()
  "Live reload reapplies package-owned chat separator face defaults."
  (should (facep 'e-chat-turn-separator-face))
  (should (facep 'e-chat-response-separator-face))
  (unwind-protect
      (progn
        (set-face-attribute 'e-chat-turn-separator-face nil
                            :foreground 'unspecified
                            :background 'unspecified
                            :box '(:line-width 1 :color "#ffffff"))
        (set-face-attribute 'e-chat-response-separator-face nil
                            :foreground 'unspecified
                            :background 'unspecified
                            :box '(:line-width 1 :color "#ffffff"))
        (e-chat--refresh-face-specs)
        (should (equal (face-attribute 'e-chat-turn-separator-face
                                       :foreground)
                       "#7f8a99"))
        (should (equal (face-attribute 'e-chat-turn-separator-face
                                       :background)
                       "#202833"))
        (should-not (face-attribute 'e-chat-turn-separator-face :box))
        (should (equal (face-attribute 'e-chat-response-separator-face
                                       :foreground)
                       "#7f8a99"))
        (should (equal (face-attribute 'e-chat-response-separator-face
                                       :background)
                       "#202833"))
        (should-not (face-attribute 'e-chat-response-separator-face :box)))
    (e-chat--refresh-face-specs)))

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
            (search-forward (concat e-chat--user-glyph " first line"))
            (should (eq (get-text-property (point) 'font-lock-face)
                        'e-chat-user-face))
            (search-forward "hello back")
            (should-not (eq (get-text-property (point) 'font-lock-face)
                            'e-chat-assistant-face)))
          (should (equal (e-chat--composer-text) "")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-shared-harness-buffers-render-only-their-session ()
  "Chat buffers attached to one harness ignore events for other sessions."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer one")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (first-buffer nil)
         (second-buffer nil))
    (unwind-protect
        (progn
          (setq first-buffer
                (e-chat-open :harness harness :session-id "chat-one"))
          (setq second-buffer
                (e-chat-open :harness harness :session-id "chat-two"))
          (e-harness-prompt harness "chat-one" "question one")
          (with-current-buffer first-buffer
            (should (string-match-p "question one" (buffer-string)))
            (should (string-match-p "answer one" (buffer-string))))
          (with-current-buffer second-buffer
            (should-not (string-match-p "question one" (buffer-string)))
            (should-not (string-match-p "answer one" (buffer-string)))))
      (when (buffer-live-p first-buffer)
        (kill-buffer first-buffer))
      (when (buffer-live-p second-buffer)
        (kill-buffer second-buffer)))))

(ert-deftest e-chat-test-submit-immediately-clears-composer-and-keeps-separator ()
  "Submitting shows the user turn and keeps an empty follow-up composer."
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
          (should (string-match-p (regexp-quote e-chat--composer-separator)
                                  (buffer-string)))
          (should (e-chat--composer-active-p))
          (should (equal (e-chat--composer-text) "")))
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
          (should (string-match-p (regexp-quote e-chat--composer-separator)
                                  (buffer-string)))
          (should (e-chat--composer-active-p))
          (should (equal (e-chat--composer-text) "")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-submit-does-not-render-assistant-before-async-provider-completes ()
  "Chat submit stays responsive while the provider request is still running."
  (let* ((finish nil)
         (backend (e-backend-create
                   :name "held-chat"
                   :start
                   (cl-function
                    (lambda (&key messages options on-item on-done on-error
                                   on-request-start)
                      (ignore messages options on-error on-request-start)
                      (setq finish
                            (lambda ()
                              (funcall on-item
                                       '(:type assistant-message
                                         :content "late answer"))
                              (funcall on-item
                                       '(:type done :reason stop))
                              (funcall on-done '(:status done))))
                      nil))))
         (harness (e-harness-create :backend backend))
         (e-chat-submit-backend-delay 0)
         (buffer (e-chat-open :harness harness
                              :session-id "chat-async-submit")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "send async")
          (e-chat-submit)
          (should (string-match-p
                   (concat (regexp-quote e-chat--user-glyph)
                           " send async")
                   (buffer-string)))
          (should-not (string-match-p "late answer" (buffer-string)))
          (should (string-match-p "queued" (format "%s" header-line-format)))
          (funcall finish)
          (e-harness-wait e-chat-harness e-chat-session-id 1.0)
          (should (string-match-p "late answer" (buffer-string)))
          (should (string-match-p "done" (format "%s" header-line-format))))
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

(ert-deftest e-chat-test-control-p-stays-inside-composer ()
  "C-p from the first composer line does not move point into transcript text."
  (let ((buffer (e-chat-test--buffer nil "chat-composer-c-p")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char e-chat--composer-start-marker)
          (let ((composer-start (marker-position e-chat--composer-start-marker)))
            (call-interactively (key-binding (kbd "C-p")))
            (should (>= (point) composer-start))
            (should-not (get-text-property (point) 'e-chat-protected))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-post-command-preserves-scrollback-position ()
  "Plain post-command handling does not force readback back to the composer."
  (let ((buffer (e-chat-test--buffer nil "chat-composer-post-command"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (goto-char (point-min))
            (set-window-start window (point-min))
            (let ((before-point (point))
                  (before-start (window-start window)))
              (run-hooks 'post-command-hook)
              (should (= (point) before-point))
              (should (= (window-start window) before-start)))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-mode-keeps-scroll-margin-user-controlled ()
  "Chat buffers do not force a global bottom scroll margin."
  (let ((scroll-margin 0)
        (buffer (e-chat-test--buffer nil "chat-composer-scroll-margin")))
    (unwind-protect
        (with-current-buffer buffer
          (should (= scroll-margin 0)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-edit-scrolls-bottom-into-view ()
  "Editing composer text scrolls the active input down after the command."
  (let ((buffer (e-chat-test--buffer nil "chat-composer-edit-scroll"))
        (window nil)
        recenter-argument)
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (cl-letf (((symbol-function 'recenter)
                     (lambda (argument &rest _ignored)
                       (setq recenter-argument argument))))
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "typed")
              (run-hooks 'post-command-hook)
              (should (equal recenter-argument -2)))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-self-insert-from-scrollback-targets-composer ()
  "Typing from readback moves to the composer before inserting text."
  (let ((buffer (e-chat-test--buffer nil "chat-composer-self-insert")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (let ((this-command 'self-insert-command)
                (last-command-event ?x))
            (run-hooks 'pre-command-hook)
            (call-interactively #'self-insert-command))
          (should (e-chat--point-in-composer-p))
          (should (equal (e-chat--composer-text) "x")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-post-command-without-edit-does-not-scroll-composer ()
  "Plain navigation commands do not force the window back to the composer."
  (let ((buffer (e-chat-test--buffer nil "chat-composer-no-edit-scroll"))
        recenter-called)
    (unwind-protect
        (cl-letf (((symbol-function 'recenter)
                   (lambda (&rest _ignored)
                     (setq recenter-called t))))
          (with-current-buffer buffer
            (run-hooks 'post-command-hook)
            (should-not recenter-called)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-show-composer-leaves-bottom-margin ()
  "Composer focus leaves a visible line between point and the mode line."
  (let ((buffer (e-chat-test--buffer nil "chat-composer-recenter"))
        (window nil)
        recenter-argument)
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (cl-letf (((symbol-function 'recenter)
                     (lambda (argument &rest _ignored)
                       (setq recenter-argument argument))))
            (with-current-buffer buffer
              (e-chat--show-composer)))
          (should (equal recenter-argument -2)))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-interactive-new-uses-selected-window-by-default ()
  "Interactive new chat opens in the selected window without a prefix argument."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend)))
         buffer
         selected-buffer)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test)
                (current-prefix-arg nil))
            (e-harness-registry-register :chat-test harness)
            (cl-letf (((symbol-function 'called-interactively-p)
                       (lambda (_kind) t))
                      ((symbol-function 'switch-to-buffer)
                       (lambda (display-buffer &rest _args)
                         (setq selected-buffer display-buffer)
                         display-buffer))
                      ((symbol-function 'pop-to-buffer)
                       (lambda (&rest _args)
                         (ert-fail "Default e-chat-new should not use pop-to-buffer"))))
              (setq buffer (call-interactively #'e-chat-new))
              (should (eq selected-buffer buffer)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-interactive-new-with-prefix-uses-pop-display ()
  "Interactive new chat uses the pop display path with a prefix argument."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend)))
         buffer
         popped-buffer)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test)
                (current-prefix-arg '(4)))
            (e-harness-registry-register :chat-test harness)
            (cl-letf (((symbol-function 'called-interactively-p)
                       (lambda (_kind) t))
                      ((symbol-function 'switch-to-buffer)
                       (lambda (&rest _args)
                         (ert-fail "Prefix e-chat-new should not use switch-to-buffer")))
                      ((symbol-function 'pop-to-buffer)
                       (lambda (display-buffer &rest _args)
                         (setq popped-buffer display-buffer)
                         display-buffer)))
              (setq buffer (call-interactively #'e-chat-new))
              (should (eq popped-buffer buffer)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-interactive-new-neutralizes-evil-after-display ()
  "Interactive display does not leave the chat buffer in Evil normal state."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend)))
         buffer)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (cl-letf (((symbol-function 'called-interactively-p)
                       (lambda (_kind) t))
                      ((symbol-function 'evil-local-mode)
                       (lambda (argument)
                         (setq-local evil-local-mode
                                     (not (and (numberp argument)
                                               (< argument 0))))
                         (unless evil-local-mode
                           (setq-local evil-state nil))))
                      ((symbol-function 'switch-to-buffer)
                       (lambda (display-buffer &rest _args)
                         (setq buffer display-buffer)
                         (with-current-buffer display-buffer
                           (setq-local evil-local-mode t)
                           (setq-local evil-state 'normal)
                           (goto-char (point-min)))
                         display-buffer)))
              (setq buffer (e-chat-new))
              (with-current-buffer buffer
                (should-not evil-local-mode)
                (should-not evil-state)
                (should (e-chat--point-in-composer-p))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-after-display-clears-chat-navigation-modes ()
  "Displaying chat returns it to a plain composer input state."
  (let ((buffer (e-chat-test--buffer nil "chat-display-input-state")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (e-chat-tool-list-mode 1)
          (e-chat--after-display-buffer buffer)
          (should-not e-chat-tool-list-mode)
          (should-not e-chat-block-view-mode)
          (should-not e-chat-response-navigation-mode)
          (should (e-chat--point-in-composer-p)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-configures-evil-initial-state-as-emacs ()
  "Chat mode declares a non-normal Evil state when Evil is available."
  (let (configured)
    (cl-letf (((symbol-function 'evil-set-initial-state)
               (lambda (mode state)
                 (setq configured (list mode state)))))
      (e-chat--configure-modal-editing-policy)
      (should (equal configured '(e-chat-mode emacs))))))

(ert-deftest e-chat-test-evil-local-mode-hook-disables-reactivation ()
  "If Evil local mode is reactivated, chat mode turns it off again."
  (let ((buffer (e-chat-test--buffer nil "chat-evil-reactivation")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local evil-local-mode t)
          (setq-local evil-state 'normal)
          (run-hooks 'evil-local-mode-hook)
          (should-not evil-local-mode)
          (should-not evil-state))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-captures-point-reference-with-two-line-context ()
  "Point capture uses the current line with two surrounding lines."
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\nfive\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((reference (e-chat--capture-context-reference)))
      (should (equal (plist-get reference :text)
                     "one\ntwo\nthree\nfour\n"))
      (should (equal (plist-get reference :start-line) 1))
      (should (equal (plist-get reference :end-line) 4))
      (should (equal (plist-get reference :point-line) 2))
      (should (string-prefix-p "buffer://" (plist-get reference :uri))))))

(ert-deftest e-chat-test-captures-region-reference-exactly ()
  "Region capture uses the exact selected text and range metadata."
  (let ((file (make-temp-file "e-chat-ref-" nil ".el")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert "alpha\nbeta\ngamma\n")
          (goto-char (point-min))
          (search-forward "beta")
          (set-mark (match-beginning 0))
          (goto-char (match-end 0))
          (setq mark-active t)
          (let ((reference (e-chat--capture-context-reference)))
            (should (equal (plist-get reference :text) "beta"))
            (should (equal (plist-get reference :start-line) 2))
            (should (equal (plist-get reference :end-line) 2))
            (should (equal (plist-get reference :point-line) 2))
            (should (equal (plist-get reference :uri)
                           (concat "file://" file)))))
      (delete-file file))))



(ert-deftest e-chat-test-attach-widens-existing-session-project-root ()
  "Attaching a session updates a too-narrow stored project root."
  (let* ((project-root (make-temp-file "e-chat-project-" t))
         (nested (expand-file-name "docs/feats/item" project-root))
         (harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" project-root) t)
          (make-directory nested t)
          (e-harness-create-session
           harness
           :id "session-1"
           :metadata (list :project-root nested))
          (let ((default-directory (file-name-as-directory nested)))
            (e-chat-open :harness harness :session-id "session-1"))
          (should (equal (e-harness-project-root harness "session-1" nil)
                         (file-name-as-directory project-root))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory project-root t))))

(ert-deftest e-chat-test-session-metadata-prefers-project-root ()
  "Chat sessions root file tools at the enclosing project, not a subdirectory."
  (let* ((project-root (make-temp-file "e-chat-project-" t))
         (nested (expand-file-name "docs/feats/item" project-root)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" project-root) t)
          (make-directory nested t)
          (let ((default-directory (file-name-as-directory nested)))
            (should (equal (plist-get (e-chat--session-metadata) :project-root)
                           (file-name-as-directory project-root)))))
      (delete-directory project-root t))))

(ert-deftest e-chat-test-submits-composer-text-with-inline-references ()
  "Submitting converts inline reference atoms into ordered prompt context."
  (let ((buffer (e-chat-test--buffer nil "chat-reference-submit")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "Look at ")
          (e-chat--insert-context-reference
           '(:id "ref-1"
             :uri "buffer://source"
             :label "source:2-4"
             :text "two\nthree\nfour\n"
             :start-line 2
             :end-line 4
             :point-line 3))
          (insert ", then explain it.")
          (e-chat-submit)
          (let* ((message (car (e-harness-messages
                                e-chat-harness
                                e-chat-session-id)))
                 (content (plist-get message :content))
                 (metadata (plist-get message :metadata)))
            (should (string-match-p
                     "Look at <reference id=\"ref-1\" label=\"source:2-4\">"
                     content))
            (should (string-match-p
                     "\\[ref-1\\] source:2-4 (buffer://source)"
                     content))
            (should (string-match-p "two\nthree\nfour" content))
            (should (equal (plist-get metadata :references)
                           '((:id "ref-1"
                              :uri "buffer://source"
                              :label "source:2-4"
                              :text "two\nthree\nfour\n"
                              :start-line 2
                              :end-line 4
                              :point-line 3))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-composer-delete-removes-inline-reference-at-boundary ()
  "Backspace and forward delete remove whole inline reference atoms."
  (let ((buffer (e-chat-test--buffer nil "chat-reference-delete")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "before ")
          (e-chat--insert-context-reference
           '(:id "ref-1"
             :uri "buffer://source"
             :label "source:2"
             :text "two"
             :start-line 2
             :end-line 2
             :point-line 2))
          (insert "after")
          (search-backward "after")
          (call-interactively (key-binding (kbd "DEL")))
          (should (equal (e-chat--composer-text) "before after"))
          (goto-char e-chat--composer-start-marker)
          (insert "again ")
          (e-chat--insert-context-reference
           '(:id "ref-2"
             :uri "buffer://source"
             :label "source:3"
             :text "three"
             :start-line 3
             :end-line 3
             :point-line 3))
          (search-backward "@[source:3]")
          (call-interactively (key-binding (kbd "C-d")))
          (should (equal (e-chat--composer-text) "again before after")))
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

(ert-deftest e-chat-test-final-assistant-face-has-no-border ()
  "Settled assistant entries use fill styling without a visible border."
  (should (equal (face-attribute 'e-chat-final-assistant-face :background)
                 "#24301f"))
  (should-not (face-attribute 'e-chat-final-assistant-face :box))
  (should (eq (face-attribute 'e-chat-final-assistant-face :extend) t)))

(ert-deftest e-chat-test-face-refresh-clears-final-assistant-border ()
  "Live reload removes older border decoration from settled assistant output."
  (let ((old-defface-spec (get 'e-chat-final-assistant-face 'face-defface-spec)))
    (unwind-protect
        (progn
          (put 'e-chat-final-assistant-face
               'face-defface-spec
               '((t :inherit e-chat-assistant-face
                    :box (:line-width 1 :color "#6f925a")
                    :extend t)))
          (set-face-attribute 'e-chat-final-assistant-face nil
                              :box '(:line-width 1 :color "#6f925a")
                              :extend nil)
          (e-chat--refresh-face-specs)
          (should (equal (face-attribute 'e-chat-final-assistant-face :background)
                         "#24301f"))
          (should-not (face-attribute 'e-chat-final-assistant-face :box))
          (should (eq (face-attribute 'e-chat-final-assistant-face :extend) t)))
      (put 'e-chat-final-assistant-face 'face-defface-spec old-defface-spec)
      (e-chat--refresh-face-specs))))

(ert-deftest e-chat-test-focused-turn-face-is-subtle ()
  "Response navigation focus uses a subdued package-owned fill only."
  (should (equal (face-attribute 'e-chat-focused-turn-face :background)
                 "#27313d"))
  (should-not (face-attribute 'e-chat-focused-turn-face :box))
  (should (eq (face-attribute 'e-chat-focused-turn-face :extend) t))
  (should-not (eq (face-attribute 'e-chat-focused-turn-face :inherit)
                  'highlight)))

(ert-deftest e-chat-test-face-refresh-clears-focused-turn-strong-decoration ()
  "Live reload removes older strong decorations from response focus."
  (let ((old-defface-spec (get 'e-chat-focused-turn-face 'face-defface-spec)))
    (unwind-protect
        (progn
          (put 'e-chat-focused-turn-face
               'face-defface-spec
               '((t :inherit highlight
                    :box (:line-width 1 :color "#3b4b5c")
                    :extend t)))
          (set-face-attribute 'e-chat-focused-turn-face nil
                              :inherit 'highlight
                              :background 'unspecified
                              :box '(:line-width 1 :color "#3b4b5c")
                              :extend nil)
          (e-chat--refresh-face-specs)
          (should (equal (face-attribute 'e-chat-focused-turn-face :background)
                         "#27313d"))
          (should-not (face-attribute 'e-chat-focused-turn-face :box))
          (should (eq (face-attribute 'e-chat-focused-turn-face :extend) t))
          (should-not (eq (face-attribute 'e-chat-focused-turn-face :inherit)
                          'highlight)))
      (put 'e-chat-focused-turn-face 'face-defface-spec old-defface-spec)
      (e-chat--refresh-face-specs))))

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

(ert-deftest e-chat-test-assistant-markdown-renders-with-text-properties ()
  "Assistant messages keep Markdown text and use markdown-mode faces."
  (skip-unless (require 'markdown-mode nil t))
  (let ((buffer (e-chat-test--buffer nil "chat-markdown")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--insert-entry
           "Assistant"
           "## Heading\nUse **bold**, *italic*, and `code`.\n- item\n\n```elisp\n(message \"hi\")\n```\n\n[docs](https://example.test)")
          (let ((content (buffer-string)))
            (should (string-match-p "## Heading" content))
            (should (string-match-p "\\*\\*bold\\*\\*" content))
            (should (string-match-p "`code`" content))
            (should (string-match-p "```elisp" content))
            (should (string-match-p "\\[docs\\](https://example.test)" content)))
          (save-excursion
            (goto-char (point-min))
            (search-forward "##")
            (should-not (get-text-property (1- (point)) 'invisible))
            (should (memq 'markdown-header-face-2
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-forward "Heading")
            (should (memq 'markdown-header-face-2
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (should-not (eq (get-text-property (1- (point)) 'font-lock-face)
                            'e-chat-assistant-face))
            (search-forward "bold")
            (should (memq 'markdown-bold-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-backward "**")
            (should (memq 'markdown-bold-face
                          (ensure-list (get-text-property (point) 'face))))
            (should-not (get-text-property (point) 'invisible))
            (search-forward "italic")
            (should (memq 'markdown-italic-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-backward "*")
            (should (memq 'markdown-italic-face
                          (ensure-list (get-text-property (point) 'face))))
            (should-not (get-text-property (point) 'invisible))
            (search-forward "code")
            (should (memq 'markdown-inline-code-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-backward "`")
            (should (memq 'markdown-inline-code-face
                          (ensure-list (get-text-property (point) 'face))))
            (should-not (get-text-property (point) 'invisible))
            (search-forward "- item")
            (should (memq 'markdown-list-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-forward "```elisp")
            (should (memq 'markdown-code-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (should-not (get-text-property (1- (point)) 'invisible))
            (search-forward "(message \"hi\")")
            (should (memq 'markdown-code-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-forward "```")
            (should (memq 'markdown-code-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (search-forward "docs")
            (should (memq 'markdown-link-face
                          (ensure-list (get-text-property (1- (point)) 'face))))
            (should (equal (get-text-property (1- (point)) 'help-echo)
                           "https://example.test"))
            (should (equal (get-text-property (1- (point)) 'e-chat-link-url)
                           "https://example.test"))
            (search-backward "[")
            (should (memq 'markdown-link-face
                          (ensure-list (get-text-property (point) 'face))))
            (should-not (get-text-property (point) 'invisible))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-hides-empty-output-diagnostic ()
  "The chat buffer renders empty backend output as an error."
  (let ((buffer (e-chat-test--buffer
                 '((:type done :reason stop))
                 "chat-empty-output")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-submit "hello")
          (e-harness-wait e-chat-harness e-chat-session-id 1.0)
          (should (string-match-p "Backend returned no assistant output"
                                  (buffer-string)))
          (should-not (string-match-p "✅ Done" (buffer-string)))
          (should-not (seq-some
                       (lambda (message)
                         (eq (plist-get message :role) 'assistant))
                       (e-harness-messages e-chat-harness e-chat-session-id)))
          (should (string-match-p "E Chat: error" header-line-format)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-abort-cancels-active-tool-request ()
  "The chat abort command cancels an active tool request."
  (let* ((e-chat-submit-backend-delay 0)
         (tool-callbacks nil)
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
                         :arguments (:text "hi")))
              (funcall on-item '(:type done :reason tool-use))
              (funcall on-done '(:status done))
              nil))))
         (tools (e-tools-registry-create))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-tool-abort")))
    (e-tools-register
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
            (e-chat-submit "run held tool"))
          (should tool-callbacks)
          (e-chat-abort)
          (funcall (plist-get tool-callbacks :on-done) "late result")
          (should (equal (plist-get
                          (e-harness-wait harness e-chat-session-id 0.1)
                          :status)
                         'cancelled))
          (should tool-cancelled)
          (should (string-match-p "Turn cancelled" (buffer-string)))
          (should (equal (mapcar (lambda (message)
                                   (plist-get message :role))
                                 (e-harness-messages harness e-chat-session-id))
                         '(user tool-call tool))))
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

(ert-deftest e-chat-test-failed-turn-expands-full-error-inline ()
  "RET on a focused failed system block expands provider details inline."
  (let ((buffer (e-chat-test--buffer nil "chat-failed-details")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
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
            (should (e-chat--block-details-visible-p block))
            (should (<= (marker-position (plist-get block :details-start-marker))
                        (point)))
            (should (<= (point)
                        (marker-position (plist-get block :details-end-marker))))
            (when (e-chat--composer-active-p)
              (should (< (point)
                         (marker-position e-chat--composer-start-marker))))
            (should-not (get-buffer e-chat-details-buffer-name))
            (should (string-match-p "OpenAI request failed" content))
            (should (string-match-p "context_length_exceeded" content))
            (should (string-match-p
                     "Your input exceeds the context window"
                     content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-turn-started-shows-moving-assistant-progress ()
  "An active assistant turn shows a protected moving glyph indicator."
  (let ((buffer (e-chat-test--buffer nil "chat-progress")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (let ((content (buffer-string)))
            (should (string-match-p
                     (concat (regexp-quote e-chat--assistant-glyph)
                             " ⠋")
                     content))
            (goto-char (point-min))
            (search-forward "⠋")
            (should (get-text-property (1- (point)) 'read-only)))
          (e-chat--advance-progress-indicator)
          (should (string-match-p
                   (concat (regexp-quote e-chat--assistant-glyph)
                           " ⠙")
                   (buffer-string)))
          (should (>= e-chat-progress-interval 0.5))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     (concat (regexp-quote e-chat--assistant-glyph)
                             " Final answer.")
                     content))
            (should-not (string-match-p
                         (concat (regexp-quote e-chat--assistant-glyph)
                                 " [⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]")
                         content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-provider-wait-and-stream-statuses ()
  "Provider lifecycle and stream events update the compact running status."
  (let ((buffer (e-chat-test--buffer nil "chat-provider-status")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat--render-event
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
          (should (equal e-chat--progress-turn-id "turn-1"))
          (e-chat--render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type reasoning-delta
                                      :content "thinking")))
          (should (string-match-p "reasoning" header-line-format))
          (e-chat--render-event
           (e-events-make :type 'assistant-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type assistant-delta
                                      :content "answer")))
          (should (string-match-p "streaming" header-line-format)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-format-duration-uses-minutes-and-seconds ()
  "Turn progress durations use minute/second labels."
  (should (equal (e-chat--format-duration 0 3) "0min 3sec"))
  (should (equal (e-chat--format-duration 0 45) "0min 45sec"))
  (should (equal (e-chat--format-duration 0 63) "1min 3sec"))
  (should (equal (e-chat--format-duration 0 205) "3min 25sec")))

(ert-deftest e-chat-test-provider-round-trip-renders-thinking-and-thought ()
  "Provider request boundaries render active and completed thinking lines."
  (let ((buffer (e-chat-test--buffer nil "chat-provider-round-trip")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _time) 8.0)))
            (e-chat--render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0
                            :payload '(:status started))))
          (should (string-match-p "⠋ Thinking for 0min 8sec"
                                  (buffer-string)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 63
                          :payload '(:status done)))
          (let ((content (buffer-string)))
            (should (string-match-p "Thought for 1min 3sec" content))
            (should-not (string-match-p "Thinking\\.\\.\\." content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-active-thinking-row-shows-spinner-and-duration ()
  "Active provider requests show a moving thinking row with current duration."
  (let ((buffer (e-chat-test--buffer nil "chat-active-thinking-duration")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _time) 8.0)))
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0))
            (e-chat--render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0
                            :payload '(:status started))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "⠋ Thinking for 0min 8sec" content))
            (should-not (string-match-p "Thinking\\.\\.\\." content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-updates-active-thinking-row ()
  "Progress redraw updates the active thinking row without duplicating it."
  (let ((buffer (e-chat-test--buffer nil "chat-active-thinking-rerender")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _time) 8.0)))
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0))
            (e-chat--render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0
                            :payload '(:status started))))
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _time) 15.0)))
            (e-chat--advance-progress-indicator))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "⠙ Thinking for 0min 15sec" content))
            (should (= (e-chat-test--count-occurrences
                        "Thinking for" content)
                       1))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-active-activity-restores-missing-progress-timer ()
  "Rendering active activity restarts a missing progress timer."
  (let ((buffer (e-chat-test--buffer nil "chat-active-thinking-timer-restore")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _time) 8.0)))
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0))
            (e-chat--render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0
                            :payload '(:status started))))
          (e-chat--cancel-progress-timer)
          (setq e-chat--progress-turn-id nil)
          (setq e-chat--progress-frame 0)
          (setq e-chat--progress-next-tick-time nil)
          (let ((record (e-chat--existing-turn-record "turn-1")))
            (should (e-chat--active-activity-p record))
            (cl-letf (((symbol-function 'float-time)
                       (lambda (&optional _time) 15.0)))
              (e-chat--render-turn-transient "turn-1" record))
            (should (equal e-chat--progress-turn-id "turn-1"))
            (should (timerp e-chat--progress-timer))
            (cl-letf (((symbol-function 'float-time)
                       (lambda (&optional _time) 16.0)))
              (e-chat--advance-progress-indicator))
            (let ((content (buffer-string)))
              (should (string-match-p "⠙ Thinking for 0min 16sec" content))
              (should (= (e-chat-test--count-occurrences
                          "Thinking for" content)
                         1)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-tool-count-renders-on-thinking-row ()
  "A round's tool count renders on the same line as its thought row."
  (let ((buffer (e-chat-test--buffer nil "chat-tool-count-thinking-row")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          (e-chat--render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:id "call-1" :name "read")))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "Thought for 0min 10sec +1 tool call" content))
            (should-not (string-match-p
                         "Thought for 0min 10sec\n\n1 tool call"
                         content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-activity-rounds-have-subtle-separators ()
  "Multiple intermittent rounds are separated inside the activity block."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-round-separators")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 20
                          :payload '(:status started)))
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
  (should (boundp 'e-chat--activity-separator))
  (should (equal e-chat--response-separator
                 e-chat--composer-separator))
  (should (equal e-chat--activity-separator
                 (make-string 64 ?┈)))
  (should-not (equal e-chat--activity-separator
                     e-chat--response-separator)))

(ert-deftest e-chat-test-activity-separator-uses-dim-activity-face ()
  "Activity round dividers use a dim face inside the activity block."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-separator-face")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:status done)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 20
                          :payload '(:status started)))
          (goto-char (point-min))
          (search-forward e-chat--activity-separator)
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
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0))
            (e-chat--render-event
             (e-events-make :type 'provider-request-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 0
                            :payload '(:status started)))
            (e-chat--render-event
             (e-events-make :type 'reasoning-delta
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 1
                            :payload '(:content "planning"))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "⠋ Thinking for 0min 8sec\n\nplanning"
                     content))
            (should-not (string-match-p
                         "⠋ Thinking for 0min 8sec\nplanning"
                         content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-provider-error-settles-thinking-line ()
  "Provider errors turn open thinking into a failed thought line."
  (let ((buffer (e-chat-test--buffer nil "chat-provider-error-thinking")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload '(:status error)))
          (e-chat--render-event
           (e-events-make :type 'turn-failed
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload '(:error "provider failed")))
          (let* ((record (e-chat--existing-turn-record "turn-1"))
                 (round (car (plist-get record :activity-records))))
            (should (equal (plist-get round :status) 'failed))
            (should (string-match-p
                     "Thought failed after 0min 12sec"
                     (e-chat--activity-expanded-text record))))
          (let ((content (buffer-string)))
            (should (string-match-p "Turn failed: provider failed" content))
            (should-not (string-match-p "Thinking\\.\\.\\." content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-turn-cancelled-settles-thinking-line ()
  "Turn cancellation turns open thinking into a cancelled thought line."
  (let ((buffer (e-chat-test--buffer nil "chat-cancelled-thinking")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'turn-cancelled
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12))
          (let* ((record (e-chat--existing-turn-record "turn-1"))
                 (round (car (plist-get record :activity-records))))
            (should (equal (plist-get round :status) 'cancelled))
            (should (string-match-p
                     "Thought cancelled after 0min 12sec"
                     (e-chat--activity-expanded-text record))))
          (let ((content (buffer-string)))
            (should (string-match-p "Turn cancelled" content))
            (should-not (string-match-p "Thinking\\.\\.\\." content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-final-turn-collapses-progress-to-summary ()
  "Settled activity collapses to a navigable turn summary."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-summary")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 63
                          :payload '(:status done)))
          (dotimes (index 2)
            (e-chat--render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 64
                            :payload (list :type 'tool-call
                                           :id (format "call-%d" index)
                                           :name "read"))))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 160
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 205
                          :payload '(:status done)))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 205
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "Turn took 3min 25sec, 2 tool calls\\." content))
            (should (string-match-p
                     (concat "Turn took 3min 25sec, 2 tool calls\\.\n\n"
                             (regexp-quote e-chat--assistant-glyph)
                             " Final answer\\.")
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

(ert-deftest e-chat-test-activity-summary-expands-to-child-blocks ()
  "Settled activity summary expansion creates navigable child blocks."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-summary-children")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:content "planning")))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 63
                          :payload '(:status done)))
          (e-chat--render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 64
                          :payload '(:id "call-1" :name "read")))
          (e-chat--render-event
           (e-events-make :type 'tool-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 65
                          :payload '(:tool-call (:id "call-1" :name "read")
                                      :result (:status ok
                                               :content "contents"))))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 160))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 205
                          :payload '(:status done)))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 205
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (goto-char (point-max))
          (insert "follow-up draft")
          (e-chat-test--focus-block-containing "Turn took 3min 25sec")
          (let* ((summary (e-chat-test--focused-block))
                 (summary-id (plist-get summary :id)))
            (call-interactively #'e-chat-response-navigation-activate)
            (let* ((children (plist-get summary :children))
                   (child-kinds
                    (mapcar
                     (lambda (block-id)
                       (plist-get (gethash block-id e-chat--block-registry)
                                  :kind))
                     children))
                   (summary-index (cl-position summary-id e-chat--block-order
                                               :test #'equal)))
              (should (equal child-kinds
                             '(activity-thought activity-reasoning
                               activity-tool-batch activity-thought)))
              (should (equal (cl-subseq e-chat--block-order
                                        (1+ summary-index)
                                        (+ 1 summary-index
                                           (length children)))
                             children))
              (should (string-match-p "follow-up draft"
                                      (buffer-string)))
              (e-chat--focus-block (car children))
              (should (equal (call-interactively
                              #'e-chat-response-navigation-copy)
                             "Thought for 1min 3sec"))
              (e-chat--focus-block (cadr children))
              (should (equal (call-interactively
                              #'e-chat-response-navigation-copy)
                             "planning"))
              (e-chat--focus-block (nth 2 children))
              (call-interactively #'e-chat-response-navigation-activate)
              (should e-chat-tool-list-mode)
              (should (= (length (plist-get (e-chat--tool-list-block)
                                            :tool-items))
                         1))
              (e-chat-tool-list-back)
              (e-chat--focus-block summary-id)
              (call-interactively #'e-chat-response-navigation-activate)
              (dolist (child children)
                (should-not (gethash child e-chat--block-registry))
                (should-not (member child e-chat--block-order))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-activity-records-track-round-ownership ()
  "Provider, reasoning, and tool events build semantic round records."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-records")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 0
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 1
                          :payload '(:content "planning")))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 63
                          :payload '(:status done)))
          (e-chat--render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 64
                          :payload '(:id "call-1"
                                      :name "read"
                                      :arguments (:path "file"))))
          (e-chat--render-event
           (e-events-make :type 'tool-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 65
                          :payload '(:tool-call (:id "call-1"
                                                :name "read")
                                      :result (:status ok
                                               :content "contents"))))
          (e-chat--render-event
           (e-events-make :type 'provider-request-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 160
                          :payload '(:status started)))
          (e-chat--render-event
           (e-events-make :type 'provider-request-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 205
                          :payload '(:status done)))
          (let* ((record (e-chat--existing-turn-record "turn-1"))
                 (rounds (plist-get record :activity-records))
                 (first-round (car rounds))
                 (second-round (cadr rounds))
                 (first-tool-batch
                  (car (plist-get first-round :tool-batches)))
                 (first-tool
                  (car (plist-get first-tool-batch :items))))
            (should (= (length rounds) 2))
            (should (equal (plist-get first-round :kind) 'round))
            (should (equal (plist-get first-round :round) 1))
            (should (equal (plist-get first-round :status) 'done))
            (should (equal (plist-get first-round :started-at) 0))
            (should (equal (plist-get first-round :ended-at) 63))
            (should (equal (plist-get (car (plist-get first-round :reasoning))
                                      :content)
                           "planning"))
            (should (equal (plist-get first-tool :id) "call-1"))
            (should (string-match-p "read" (plist-get first-tool :call)))
            (should (string-match-p "contents" (plist-get first-tool :output)))
            (should (equal (plist-get second-round :round) 2))
            (should (null (plist-get second-round :tool-batches)))
            (plist-put record :intermittent-entries nil)
            (plist-put record :ended-at 205)
            (plist-put record :final-rendered t)
            (should (equal (e-chat--activity-summary-text record)
                           "Turn took 3min 25sec, 1 tool call."))
            (should (string-match-p
                     "Thought for 1min 3sec"
                     (e-chat--activity-expanded-text record)))
            (should (= (length (e-chat--activity-tool-items record)) 1))))
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
            (let* ((record (e-chat--existing-turn-record "turn-1"))
                   (rounds (plist-get record :activity-records))
                   (tool-batch (car (plist-get (car rounds) :tool-batches)))
                   (summary (e-chat-test--focused-block))
                   (child-kinds
                    (mapcar
                     (lambda (block-id)
                       (plist-get (gethash block-id e-chat--block-registry)
                                  :kind))
                     (plist-get summary :children))))
              (should (= (length rounds) 1))
              (should (equal (plist-get (car rounds) :status) 'done))
              (should (= (length (plist-get tool-batch :items)) 1))
              (should (equal child-kinds
                             '(activity-thought activity-tool-batch))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-late-progress-tick-shows-emacs-blocked-status ()
  "A delayed progress timer tick only reports a local Emacs stall."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-stall"))
        (e-chat-progress-interval 0.5))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (setq e-chat--progress-next-tick-time
                (- (float-time) 7.0))
          (e-chat--advance-progress-indicator)
          (should (string-match-p
                   "Emacs was blocked for [0-9]+s; checking turn state"
                   header-line-format))
          (should (equal e-chat--progress-turn-id "turn-1")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-keeps-editable-composer-draft ()
  "Progress redraws keep the follow-up composer editable with draft text."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-editable-composer")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (should (e-chat--composer-active-p))
          (goto-char (point-max))
          (insert "follow-up draft")
          (e-chat--advance-progress-indicator)
          (should (e-chat--composer-active-p))
          (should (equal (e-chat--composer-text) "follow-up draft")))
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
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-2"
                            :created-at 20))
            (goto-char (point-max))
            (insert "follow-up draft")
            (goto-char (point-min))
            (set-window-point window (point))
            (set-window-start window (point))
            (let ((before-point (point))
                  (before-window-point (window-point window))
                  (before-window-start (window-start window)))
              (e-chat--advance-progress-indicator)
              (should (= (point) before-point))
              (should (= (window-point window) before-window-point))
              (should (= (window-start window) before-window-start))
              (should (e-chat--composer-active-p))
              (should (equal (e-chat--composer-text) "follow-up draft")))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-strips-composer-presentation-properties ()
  "Progress redraws do not leak transcript presentation into composer text."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-composer-properties")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (should (e-chat--composer-active-p))
          (goto-char (point-max))
          (let ((start (point)))
            (insert "follow-up draft")
            (add-text-properties
             start
             (point)
             '(font-lock-face e-chat-user-face
               e-chat-block-id "leaked-block"
               e-chat-turn-id "leaked-turn"))
            (e-chat--advance-progress-indicator)
            (goto-char e-chat--composer-start-marker)
            (search-forward "follow-up draft")
            (let ((position (match-beginning 0)))
              (should-not (get-text-property position 'font-lock-face))
              (should-not (get-text-property position 'e-chat-block-id))
              (should-not (get-text-property position 'e-chat-turn-id)))
            (should (equal (e-chat--composer-text) "follow-up draft"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-preserves-context-reference ()
  "Progress redraws keep inline context reference properties in the composer."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-context-reference")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (goto-char (point-max))
          (insert "Review ")
          (let ((reference
                 (e-chat--insert-context-reference
                  '(:id "ref-1"
                    :uri "buffer://source"
                    :label "source:2"
                    :text "two"
                    :start-line 2
                    :end-line 2
                    :point-line 2))))
            (insert " before replying.")
            (e-chat--advance-progress-indicator)
            (should (e-chat--composer-active-p))
            (let ((document (e-chat--composer-document)))
              (should (equal (plist-get document :text)
                             "Review <reference id=\"ref-1\" label=\"source:2\"> before replying."))
              (should (equal (plist-get document :references)
                             (list reference))))
            (goto-char e-chat--composer-start-marker)
            (search-forward "@[source:2]")
            (should (equal (get-text-property (match-beginning 0)
                                              'e-chat-context-reference)
                           reference))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-submit-keeps-follow-up-composer-editable ()
  "Submitting a prompt leaves an empty editable follow-up composer."
  (let* ((backend (e-backend-fake-create :items nil :delay 1.0))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open
                  :harness harness
                  :session-id "chat-submit-follow-up-composer")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "Initial prompt")
          (e-chat-submit)
          (should (e-chat--composer-active-p))
          (should (equal (e-chat--composer-text) ""))
          (goto-char (point-max))
          (insert "follow-up draft")
          (should (equal (e-chat--composer-text) "follow-up draft"))
          (e-chat--advance-progress-indicator)
          (should (equal (e-chat--composer-text) "follow-up draft")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-running-activity-forces-redisplay ()
  "Running turn activity forces Emacs to repaint before the final response."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-redisplay"))
        redisplays)
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'redisplay)
                     (lambda (&optional force)
                       (push force redisplays))))
            (e-chat--render-event
             (e-events-make :type 'turn-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :created-at 10))
            (e-chat--render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload '(:type tool-call
                                        :id "call-1"
                                        :name "read"
                                        :arguments (:uri "file://x")))))
          (should (equal redisplays '(t t)))
          (let ((content (buffer-string)))
            (should (string-match-p "1 tool call" content))
            (should-not (string-match-p "Working" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-frame-forces-redisplay ()
  "Advancing the assistant progress indicator forces Emacs to repaint."
  (let ((buffer (e-chat-test--buffer nil "chat-progress-redisplay"))
        redisplays)
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (setq redisplays nil)
          (cl-letf (((symbol-function 'redisplay)
                     (lambda (&optional force)
                       (push force redisplays))))
            (e-chat--advance-progress-indicator))
          (should (equal redisplays '(t))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-final-response-uses-visual-marker-face ()
  "Final assistant output is visually distinguished without a text label."
  (let ((buffer (e-chat-test--buffer nil "chat-final-face")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (goto-char (point-min))
          (search-forward "Final answer.")
          (should (memq 'e-chat-final-assistant-face
                        (ensure-list
                         (get-text-property (1- (point)) 'face))))
          (should-not (string-match-p "\nFinal\n" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-final-response-keeps-markdown-faces ()
  "Settled assistant styling preserves Markdown presentation faces."
  (let ((buffer (e-chat-test--buffer nil "chat-final-markdown")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--insert-entry
           "Assistant"
           "Use **bold** and `code`.")
          (goto-char (point-min))
          (search-forward "bold")
          (let ((faces (ensure-list
                        (get-text-property (1- (point)) 'face))))
            (should (memq 'e-chat-final-assistant-face faces))
            (should (memq 'e-chat-markdown-strong-face faces))
            (should-not (eq (get-text-property (1- (point)) 'font-lock-face)
                            'e-chat-final-assistant-face)))
          (search-forward "code")
          (let ((faces (ensure-list
                        (get-text-property (1- (point)) 'face))))
            (should (memq 'e-chat-final-assistant-face faces))
            (should (memq 'e-chat-markdown-code-face faces))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-details-shows-intermittent-events ()
  "Details buffer shows intermittent reasoning before metadata."
  (let ((buffer (e-chat-test--buffer nil "chat-intermittent-expand")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat--render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type reasoning-delta
                                      :content "Need current buffer state.")))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (e-chat--render-event
           (e-events-make :type 'turn-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload '(:reason stop)))
          (call-interactively #'e-chat-enter-response-navigation)
          (let ((details (e-chat-response-navigation-details)))
            (with-current-buffer details
              (should (string-match-p
                       "Reasoning\n  Need current buffer state\\.\n\n  Turn: turn-1"
                       (buffer-string))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-buffer-name e-chat-details-buffer-name))))

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

(ert-deftest e-chat-test-response-navigation-ret-enters-block-view ()
  "RET on a final block enters block-local view and ESC returns to navigation."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-expand")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat-test--render-turn "turn-2" 20 23.5 "second" "two")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (should e-chat-block-view-mode)
          (should-not e-chat-response-navigation-mode)
          (should (equal (plist-get (e-chat-test--focused-block) :action-text)
                         "two"))
          (call-interactively #'e-chat-block-view-back)
          (should-not e-chat-block-view-mode)
          (should e-chat-response-navigation-mode)
          (should (equal e-chat--focused-turn-id "turn-2")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-block-view-i-returns-to-composer ()
  "Pressing i in block view leaves navigation and focuses the composer."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-fold")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (call-interactively #'e-chat-block-view-insert)
          (should-not e-chat-block-view-mode)
          (should-not e-chat-response-navigation-mode)
          (should (>= (point) (marker-position e-chat--composer-start-marker))))
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
          (let ((bounds (e-chat--block-content-bounds
                         (e-chat--block-view-block))))
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

(ert-deftest e-chat-test-block-view-can-select-and-copy-text ()
  "Block view v starts a region, h/l keep it active, and y copies it."
  (let ((buffer (e-chat-test--buffer nil "chat-block-view-select")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "alpha beta")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (call-interactively
           (lookup-key e-chat-block-view-mode-map (kbd "v")))
          (dotimes (_ 5)
            (call-interactively
             (lookup-key e-chat-block-view-mode-map (kbd "l"))))
          (should (region-active-p))
          (call-interactively
           (lookup-key e-chat-block-view-mode-map (kbd "y")))
          (should (equal (current-kill 0) "alpha")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-block-view-esc-clears-selection-before-exiting ()
  "In block-view selection mode, ESC resets selection before returning to nav."
  (let ((buffer (e-chat-test--buffer nil "chat-block-view-selection-esc")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "alpha beta")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (call-interactively
           (lookup-key e-chat-block-view-mode-map (kbd "v")))
          (dotimes (_ 5)
            (call-interactively
             (lookup-key e-chat-block-view-mode-map (kbd "l"))))
          (should (region-active-p))
          (call-interactively
           (lookup-key e-chat-block-view-mode-map (kbd "<escape>")))
          (should-not (region-active-p))
          (should e-chat-block-view-mode)
          (should-not e-chat-response-navigation-mode)
          (call-interactively
           (lookup-key e-chat-block-view-mode-map (kbd "<escape>")))
          (should-not e-chat-block-view-mode)
          (should e-chat-response-navigation-mode))
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

(ert-deftest e-chat-test-response-navigation-copy-and-open-use-block-content ()
  "Copy and open actions use the focused block action text without chrome."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-actions"))
        (opened nil))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first prompt" "final text")
          (e-chat-test--focus-block-containing "first prompt")
          (should (eq (plist-get (e-chat-test--focused-block) :kind) 'user))
          (call-interactively #'e-chat-response-navigation-copy)
          (should (equal (current-kill 0) "first prompt"))
          (setq opened (e-chat-response-navigation-open))
          (with-current-buffer opened
            (should (derived-mode-p 'text-mode))
            (should-not buffer-read-only)
            (should (equal (buffer-string) "first prompt")))
          (with-current-buffer buffer
            (e-chat-test--focus-block-containing "final text")
            (should (eq (plist-get (e-chat-test--focused-block) :kind) 'final))
            (call-interactively #'e-chat-response-navigation-copy)
            (should (equal (current-kill 0) "final text"))))
      (when (buffer-live-p opened)
        (kill-buffer opened))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-reattach-does-not-duplicate-rendered-events ()
  "Reattaching a chat buffer leaves one live subscription for the session."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (buffer (e-chat-open :harness harness :session-id "chat-reattach")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--attach-buffer buffer harness "chat-reattach")
          (should (= (e-chat-test--session-subscriber-count
                      harness "chat-reattach")
                     1))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload (list :message
                                         (list :role 'user
                                               :content "dup prompt"))))
          (should (= (e-chat-test--count-occurrences
                      "dup prompt"
                      (buffer-string))
                     1))
          (should (= (length e-chat--block-order) 1)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-details-open-read-only-buffer ()
  "Details command opens turn metadata and tool details outside the chat buffer."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-details")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "inspect" "done")
          (e-chat--record-activity-event
           "turn-1"
           '(:event-type tool-started
             :payload (:type tool-call :name "buffer-read"
                       :arguments (:buffer "*scratch*"))))
          (e-chat--record-activity-event
           "turn-1"
           '(:event-type tool-finished
             :payload (:result (:status ok :content "scratch contents"))))
          (e-chat-test--focus-block-containing "done")
          (let ((details (e-chat-response-navigation-details)))
            (with-current-buffer details
              (should (derived-mode-p 'special-mode))
              (should buffer-read-only)
              (should (string-match-p "Turn: turn-1" (buffer-string)))
              (should (string-match-p "buffer-read" (buffer-string)))
              (should (string-match-p "scratch contents" (buffer-string)))
              (should-error (insert "mutate") :type 'buffer-read-only))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-buffer-name e-chat-details-buffer-name))))

(ert-deftest e-chat-test-running-activity-is-navigable-while-progress-active ()
  "Running activity summary blocks are navigable before the turn settles."
  (let ((buffer (e-chat-test--buffer nil "chat-running-activity-nav")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat--render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type tool-call
                                      :id "call-1"
                                      :name "read")))
          (goto-char (point-min))
          (search-forward "1 tool call")
          (call-interactively #'e-chat-enter-response-navigation)
          (should (eq (plist-get (e-chat-test--focused-block) :kind)
                      'activity))
          (call-interactively #'e-chat-response-navigation-activate)
          (should e-chat-tool-list-mode)
          (should (e-chat--composer-active-p)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-preserves-response-navigation ()
  "Progress redraws keep response navigation focused on the same block."
  (let ((buffer (e-chat-test--buffer nil "chat-running-nav-preserve")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-2"
                          :created-at 20))
          (goto-char (point-min))
          (search-forward "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (let ((focused-block-id e-chat--focused-block-id)
                (focused-point (point)))
            (e-chat--advance-progress-indicator)
            (should e-chat-response-navigation-mode)
            (should (equal e-chat--focused-block-id focused-block-id))
            (should (= (point) focused-point))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-preserves-block-view ()
  "Progress redraws keep block view point inside the active block."
  (let ((buffer (e-chat-test--buffer nil "chat-running-block-preserve")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one two three")
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-2"
                          :created-at 20))
          (goto-char (point-min))
          (search-forward "one two")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (let* ((block-id e-chat--block-view-block-id)
                 (bounds (e-chat--block-content-bounds
                          (e-chat--block-view-block)))
                 (target-point (+ (car bounds) 4)))
            (goto-char target-point)
            (e-chat--advance-progress-indicator)
            (should e-chat-block-view-mode)
            (should (equal e-chat--block-view-block-id block-id))
            (should (= (point) target-point))
            (let ((updated-bounds (e-chat--block-content-bounds
                                   (e-chat--block-view-block))))
              (should (<= (car updated-bounds) (point)))
              (should (<= (point) (cdr updated-bounds))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-rerender-preserves-running-tool-list ()
  "Progress redraws keep a running activity tool list and selected item."
  (let ((buffer (e-chat-test--buffer nil "chat-running-tool-list-preserve")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (dolist (tool '(("call-1" "read")
                          ("call-2" "write")))
            (e-chat--render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload (list :type 'tool-call
                                           :id (nth 0 tool)
                                           :name (nth 1 tool)))))
          (goto-char (point-min))
          (search-forward "2 tool calls")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (should e-chat-tool-list-mode)
          (call-interactively #'e-chat-tool-list-next)
          (should (= e-chat--tool-list-index 1))
          (e-chat--advance-progress-indicator)
          (should e-chat-tool-list-mode)
          (should (= e-chat--tool-list-index 1))
          (should (string-match-p "write"
                                  (buffer-substring-no-properties
                                   (overlay-start e-chat--tool-list-overlay)
                                   (overlay-end e-chat--tool-list-overlay)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-finalizing-tool-activity-removes-dead-blocks ()
  "Completed tool activity does not leave zero-width blocks in navigation."
  (let ((buffer (e-chat-test--buffer nil "chat-finalize-tool-activity")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload (list :message
                                         (list :role 'user
                                               :content "inspect prompt"))))
          (e-chat--render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload (list :type 'tool-call
                                         :id "call-1"
                                         :name "read")))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 12
                          :payload (list :message
                                         (list :role 'assistant
                                               :content "final answer"))))
          (should (cl-every #'e-chat--live-block-record e-chat--block-order))
          (should-not (cl-some
                       (lambda (block-id)
                         (eq (plist-get (gethash block-id e-chat--block-registry)
                                        :kind)
                             'activity))
                       e-chat--block-order)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-response-navigation-skips-dead-activity-to-user-block ()
  "Block navigation from a final response can reach and enter the user prompt."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-skip-dead-activity")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload (list :message
                                         (list :role 'user
                                               :content "copy this prompt"))))
          (e-chat--render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload (list :type 'tool-call
                                         :id "call-1"
                                         :name "read")))
          (e-chat--render-event
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
          (should (equal (e-chat--block-action-text (e-chat--block-view-block))
                         "copy this prompt")))
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
           store "chat-nav-replay"
           '(:role user
             :content "old first"
             :created-at "1970-01-01T00:00:10Z"))
          (e-session-append-message
           store "chat-nav-replay"
           '(:role assistant
             :content "old one"
             :created-at "1970-01-01T00:00:12Z"))
          (e-session-append-message
           store "chat-nav-replay"
           '(:role user
             :content "old second"
             :created-at "1970-01-01T00:00:20Z"))
          (e-session-append-message
           store "chat-nav-replay"
           '(:role assistant
             :content "old two"
             :created-at "1970-01-01T00:00:22Z"))
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-nav-replay"))
          (with-current-buffer buffer
            (call-interactively #'e-chat-enter-response-navigation)
            (should (equal e-chat--focused-turn-id "replayed-turn-2"))
            (let ((details (e-chat-response-navigation-details)))
              (with-current-buffer details
                (should (string-match-p
                         "  Started: 1970-01-01T00:00:20Z"
                         (buffer-string)))
                (should (string-match-p
                         "  Ended: 1970-01-01T00:00:22Z"
                         (buffer-string)))
                (should (string-match-p "  Duration: 0min 2sec"
                                        (buffer-string)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-buffer-name e-chat-details-buffer-name))))

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
              (should (e-chat--block-details-visible-p block))
              (should (<= (marker-position (plist-get block :details-start-marker))
                          (point)))
              (should (<= (point)
                          (marker-position (plist-get block :details-end-marker))))
              (when (e-chat--composer-active-p)
                (should (< (point)
                           (marker-position e-chat--composer-start-marker))))
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
          (setq buffer (e-chat-open
                        :harness harness
                        :session-id "chat-failed-thinking-replay"))
          (with-current-buffer buffer
            (let* ((record (e-chat--existing-turn-record "turn-1"))
                   (round (car (plist-get record :activity-records))))
              (should (equal (plist-get round :status) 'failed))
              (should (string-match-p
                       "Thought failed after [0-9]+min [0-9]+sec"
                       (e-chat--activity-expanded-text record))))
            (let ((content (buffer-string)))
              (should (string-match-p "Turn failed: provider failed" content))
              (should-not (string-match-p "Thinking\\.\\.\\." content)))))
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

(ert-deftest e-chat-test-composer-spacer-shrinks-for-multiline-input ()
  "Growing composer input reduces the protected bottom spacer."
  (let ((buffer (e-chat-test--buffer nil "chat-bottom-multiline"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (let ((e-chat--test-window-body-height 12)
                  (e-chat--test-transcript-screen-lines 4))
              (e-chat--refresh-composer-position))
            (let ((single-line-spacer
                   (count-lines e-chat--composer-spacer-marker
                                e-chat--transcript-end-marker)))
              (goto-char (point-max))
              (insert "one\ntwo\nthree")
              (let ((e-chat--test-window-body-height 12)
                    (e-chat--test-transcript-screen-lines 4))
                (e-chat--refresh-composer-position))
              (let ((multiline-spacer
                     (count-lines e-chat--composer-spacer-marker
                                  e-chat--transcript-end-marker)))
                (should (< multiline-spacer single-line-spacer))))))
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

(ert-deftest e-chat-test-window-change-refreshes-visible-composer-spacer ()
  "Window changes recalculate the composer spacer for visible chat buffers."
  (let ((buffer (e-chat-test--buffer nil "chat-resize"))
        (window nil))
    (unwind-protect
        (progn
          (setq window (display-buffer buffer))
          (with-current-buffer buffer
            (let ((e-chat--test-window-body-height 18))
              (e-chat--refresh-composer-position))
            (let ((before (count-lines e-chat--composer-spacer-marker
                                       e-chat--transcript-end-marker)))
              (let ((e-chat--test-window-body-height 8))
                (e-chat--refresh-visible-composers))
              (let ((after (count-lines e-chat--composer-spacer-marker
                                        e-chat--transcript-end-marker)))
                (should (< after before))))))
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
         (new-harness (e-chat-test--activate-chat-session
                       (e-harness-create :backend new-backend :sessions store)))
         (buffer (e-chat-open :harness old-harness :session-id "chat-reload")))
    (unwind-protect
        (progn
          (e-session-append-message
           store "chat-reload" '(:id "msg-1" :role user :content "saved prompt"))
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "stale prompt"))
          (e-chat-test--with-empty-harness-registry
            (let ((e-chat-default-harness-id :chat-test))
              (e-harness-registry-register-factory
               :chat-test
               (lambda () new-harness))
              (should (= (e-chat-reload-buffers) 1))))
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

(ert-deftest e-chat-test-default-harness-requests-configured-registry-id ()
  "Chat shell asks the harness registry for the configured default id."
  (e-chat-test--with-empty-harness-registry
    (let* ((e-chat-default-harness-id :chat-test)
           (harness (e-chat-test--activate-chat-session
                     (e-harness-create
                      :backend (e-backend-fake-create :items nil)))))
      (e-harness-registry-register :chat-test harness)
      (should (eq (e-chat--default-harness) harness)))))

(ert-deftest e-chat-test-default-harness-missing-id-is-user-error ()
  "Missing configured harness ids surface as chat command errors."
  (e-chat-test--with-empty-harness-registry
    (let ((e-chat-default-harness-id :missing-chat))
      (should-error (e-chat--default-harness) :type 'user-error))))

(ert-deftest e-chat-test-default-harness-requires-chat-session ()
  "The configured default harness must expose the chat-session capability."
  (e-chat-test--with-empty-harness-registry
    (let ((e-chat-default-harness-id :chat-test)
          (harness (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-harness-registry-register :chat-test harness)
      (should-error (e-chat--default-harness) :type 'user-error))))

(ert-deftest e-chat-test-source-does-not-cross-harness-boundaries ()
  "Chat shell uses registry lookup and public harness projections."
  (let ((source (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "lisp/shells/chat/e-chat.el"
                                     (e-source-directory)))
                  (buffer-string))))
    (dolist (forbidden '("e-openai-create-harness"
                         "e-harness--turn-options"
                         "e-session-display-title"
                         "e-session-list"
                         "e-session-activity-events"
                         "e-session-get"))
      (should-not (string-match-p forbidden source)))))

(ert-deftest e-chat-test-new-creates-distinct-persisted-sessions ()
  "Each new chat command invocation creates a distinct persisted session."
  (let* ((directory (make-temp-file "e-chat-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         first-id second-id)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
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
                     (expand-file-name
                      (concat second-id ".jsonl")
                      (expand-file-name "sessions" directory))))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))

(ert-deftest e-chat-test-resume-selects-existing-session ()
  "Resuming uses completing-read over persisted sessions and renders transcript."
  (let* ((directory (make-temp-file "e-chat-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store))))
    (unwind-protect
        (progn
          (e-session-create store :id "resume-me")
          (e-session-append-message
           store "resume-me" '(:id "msg-1" :role user :content "saved hello"))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _args)
                       (car collection))))
            (e-chat-test--with-empty-harness-registry
              (let ((e-chat-default-harness-id :chat-test))
                (e-harness-registry-register :chat-test harness)
                (with-current-buffer (e-chat-resume)
                  (should (equal e-chat-session-id "resume-me"))
                  (should (string-match-p "saved hello" (buffer-string))))))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))

(ert-deftest e-chat-test-resume-preview-state-renders-selected-session ()
  "Resume completion preview renders the highlighted session in a preview buffer."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (origin (get-buffer-create "chat-resume-preview-origin")))
    (unwind-protect
        (progn
          (e-session-create store :id "preview-me")
          (e-session-append-message
           store "preview-me" '(:id "msg-1" :role user :content "preview hello"))
          (let* ((sessions (e-harness-session-list harness))
                 (labels (mapcar #'e-chat--session-choice-label sessions))
                 (state (e-chat--resume-preview-state harness sessions labels)))
            (switch-to-buffer origin)
            (funcall state 'preview (car labels))
            (let ((preview (get-buffer e-chat--resume-preview-buffer-name)))
              (should preview)
              (should (eq (window-buffer (selected-window)) preview))
              (with-current-buffer preview
                (should (equal e-chat-session-id "preview-me"))
                (should (string-match-p "preview hello" (buffer-string)))))
            (funcall state 'exit nil)
            (should (eq (window-buffer (selected-window)) origin))
            (should-not (get-buffer e-chat--resume-preview-buffer-name))))
      (e-chat-test--kill-chat-buffers)
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(ert-deftest e-chat-test-resume-preview-renders-only-tail-messages ()
  "Resume previews render a small transcript tail for responsive selection."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (origin (get-buffer-create "chat-resume-preview-tail-origin"))
         (e-chat-resume-preview-message-limit 2))
    (unwind-protect
        (progn
          (e-session-create store :id "preview-tail"
                            :metadata '(:name "Tail preview"))
          (dotimes (index 6)
            (e-session-append-message
             store
             "preview-tail"
             (list :id (format "msg-%d" index)
                   :role (if (cl-evenp index) 'user 'assistant)
                   :content (format "preview message %d" index))))
          (let* ((sessions (e-harness-session-list harness))
                 (labels (mapcar #'e-chat--session-choice-label sessions))
                 (state (e-chat--resume-preview-state harness sessions labels)))
            (switch-to-buffer origin)
            (funcall state 'preview (car labels))
            (let ((preview (get-buffer e-chat--resume-preview-buffer-name)))
              (should preview)
              (with-current-buffer preview
                (let ((text (buffer-string)))
                  (should-not (string-match-p "preview message 0" text))
                  (should-not (string-match-p "preview message 3" text))
                  (should (string-match-p "preview message 4" text))
                  (should (string-match-p "preview message 5" text)))))
            (funcall state 'exit nil)))
      (e-chat-test--kill-chat-buffers)
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(ert-deftest e-chat-test-resume-preview-for-index-session-avoids-transcript-load ()
  "Resume previews render metadata when a persistent transcript is not loaded."
  (let* ((directory (make-temp-file "e-chat-" t))
         (store (e-session-persistent-store-create directory))
         (session-id (plist-get
                      (e-session-create store
                                        :id "indexed-preview"
                                        :metadata '(:name "Indexed preview"))
                      :id))
         (backend (e-backend-fake-create :items nil)))
    (unwind-protect
        (progn
          (e-session-append-message
           store session-id
           '(:id "msg-1" :role user :content "indexed preview hello"))
          (let* ((indexed-store (e-session-persistent-index-store-create directory))
                 (harness (e-chat-test--activate-chat-session
                           (e-harness-create :backend backend
                                             :sessions indexed-store)))
                 (session (car (e-harness-session-list harness)))
                 (loaded nil))
            (should-not (plist-get session :loaded))
            (cl-letf (((symbol-function 'e-session-load-session)
                       (lambda (&rest _args)
                         (setq loaded t)
                         (error "preview loaded transcript"))))
              (let ((preview (e-chat--render-resume-preview harness session)))
                (should-not loaded)
                (with-current-buffer preview
                  (let ((text (buffer-string)))
                    (should buffer-read-only)
                    (should (equal e-chat-session-id "indexed-preview"))
                    (should-not (string-match-p
                                 (regexp-quote e-chat--composer-glyph)
                                 text))
                    (should (string-match-p "Indexed preview" text))
                    (should (string-match-p "indexed preview hello" text)))))
              (let ((preview (e-chat--render-resume-preview harness session)))
                (should-not loaded)
                (with-current-buffer preview
                  (let ((text (buffer-string)))
                    (should buffer-read-only)
                    (should (equal e-chat-session-id "indexed-preview"))
                    (should-not (string-match-p
                                 (regexp-quote e-chat--composer-glyph)
                                 text))
                    (should (string-match-p "Indexed preview" text))))))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))

(ert-deftest e-chat-test-resume-reader-uses-consult-preview-when-available ()
  "Resume selection uses Consult preview state when Consult is available."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         selected-state selected-sort)
    (e-session-create store :id "resume-me")
    (e-session-append-message
     store "resume-me" '(:id "msg-1" :role user :content "saved hello"))
    (let ((original-require (symbol-function 'require)))
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &optional filename noerror)
                   (if (eq feature 'consult)
                       t
                     (funcall original-require feature filename noerror))))
                ((symbol-function 'consult--read)
                 (lambda (collection &rest options)
                   (setq selected-state (plist-get options :state))
                   (setq selected-sort (plist-get options :sort))
                   (car collection))))
        (let* ((sessions (e-harness-session-list harness))
               (labels (mapcar #'e-chat--session-choice-label sessions)))
          (should (equal (e-chat--read-session-choice harness sessions)
                         (car labels)))
          (should (functionp selected-state))
          (should (eq selected-sort nil)))))))

(ert-deftest e-chat-test-overview-renders-sessions-in-recency-order ()
  "Overview rows render latest sessions first and mark unread sessions."
  (let* ((directory (make-temp-file "e-chat-overview-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (e-chat-overview-state-file
          (expand-file-name "overview-state.json" directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "older-session"
                            :metadata '(:name "Older"))
          (e-session-append-message
           store "older-session"
           '(:id "old-assistant" :role assistant :content "older answer"))
          (e-session-create store :id "newer-session"
                            :metadata '(:name "Newer"))
          (e-session-append-message
           store "newer-session"
           '(:id "new-assistant" :role assistant :content "newer answer"))
          (let ((buffer (get-buffer-create "*e-chat-overview-test*")))
            (unwind-protect
                (with-current-buffer buffer
                  (e-chat-overview-mode)
                  (e-chat-overview--render harness)
                  (let* ((text (buffer-string))
                         (newer-pos (string-match-p "Newer" text))
                         (older-pos (string-match-p "Older" text)))
                    (should newer-pos)
                    (should older-pos)
                    (should (< newer-pos older-pos))
                    (should (string-match-p "! Newer" text))
                    (should (string-match-p "! Older" text))))
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))

(ert-deftest e-chat-test-overview-open-session-marks-session-read ()
  "Opening from overview records the selected session read marker."
  (let* ((directory (make-temp-file "e-chat-overview-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (e-chat-overview-state-file
          (expand-file-name "overview-state.json" directory)))
    (unwind-protect
        (progn
          (e-session-create store :id "read-me"
                            :metadata '(:name "Read Me"))
          (e-session-append-message
           store "read-me"
           '(:id "assistant-read" :role assistant :content "answer"))
          (let ((buffer (get-buffer-create "*e-chat-overview-test*")))
            (unwind-protect
                (with-current-buffer buffer
                  (e-chat-overview-mode)
                  (e-chat-overview--render harness)
                  (goto-char (point-min))
                  (let ((chat-buffer (e-chat-overview-open-session)))
                    (with-current-buffer chat-buffer
                      (should (equal e-chat-session-id "read-me")))
                    (should (equal
                             (e-chat-overview--read-marker "read-me")
                             "assistant-read"))
                    (e-chat-overview--render harness)
                    (should-not (string-match-p
                                 "! Read Me"
                                 (buffer-string)))))
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))))
      (e-chat-test--kill-chat-buffers)
      (delete-directory directory t))))

(ert-deftest e-chat-test-add-context-to-latest-targets-visible-session ()
  "Latest context insertion targets a visible chat before recency."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         window)
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-session-create store :id "visible-session"
                              :metadata '(:name "visible-session"))
            (setq window
                  (display-buffer
                   (e-chat-open :harness harness :session-id "visible-session")))
            (e-session-create store :id "latest-session"
                              :metadata '(:name "latest-session"))
            (with-temp-buffer
              (insert "alpha\nbeta\ngamma\n")
              (goto-char (point-min))
              (forward-line 1)
              (let ((chat-buffer (e-chat-add-context-to-latest)))
                (should (eq chat-buffer (window-buffer window)))
                (with-current-buffer chat-buffer
                  (should (equal e-chat-session-id "visible-session"))
                  (should (string-match-p
                           "@\\[.*:1-3\\]"
                           (buffer-substring-no-properties
                            e-chat--composer-start-marker
                            (point-max)))))))))
      (when (and window (window-live-p window))
        (delete-window window))
      (e-chat-test--kill-chat-buffers))))

(ert-deftest e-chat-test-add-context-to-latest-falls-back-to-most-recent-session ()
  "Latest context insertion opens the most recently updated chat session."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store))))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-session-create store :id "old-session"
                              :metadata '(:name "old-session"))
            (e-session-create store :id "latest-session"
                              :metadata '(:name "latest-session"))
            (with-temp-buffer
              (insert "alpha\nbeta\ngamma\n")
              (goto-char (point-min))
              (forward-line 1)
              (let ((chat-buffer (e-chat-add-context-to-latest)))
                (with-current-buffer chat-buffer
                  (should (equal e-chat-session-id "latest-session"))
                  (should (string-match-p
                           "latest-session"
                           (buffer-name)))
                  (should (string-match-p
                           "@\\[.*:1-3\\]"
                           (buffer-substring-no-properties
                            e-chat--composer-start-marker
                            (point-max)))))))))
      (e-chat-test--kill-chat-buffers))))

(ert-deftest e-chat-test-add-context-to-latest-deactivates-source-region ()
  "Latest context insertion clears the selected region in the source buffer."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store))))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-session-create store :id "latest-session"
                              :metadata '(:name "latest-session"))
            (with-temp-buffer
              (insert "alpha beta gamma")
              (goto-char (point-min))
              (search-forward "beta")
              (set-mark (match-beginning 0))
              (setq mark-active t)
              (let ((chat-buffer (e-chat-add-context-to-latest)))
                (should-not mark-active)
                (with-current-buffer chat-buffer
                  (should (string-match-p
                           "@\\[.*:1\\]"
                           (buffer-substring-no-properties
                            e-chat--composer-start-marker
                            (point-max)))))))))
      (e-chat-test--kill-chat-buffers))))

(ert-deftest e-chat-test-add-context-clears-target-block-view-mode ()
  "Context insertion into a chat target exits stale block view state."
  (let ((buffer (e-chat-test--buffer nil "chat-context-block-view"))
        (reference '(:uri "buffer://source"
                     :label "source:1"
                     :text "source"
                     :start-line 1
                     :end-line 1
                     :point-line 1)))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (call-interactively #'e-chat-response-navigation-activate)
          (should e-chat-block-view-mode)
          (e-chat--add-context-reference-to-session
           reference
           e-chat-harness
           e-chat-session-id)
          (should-not e-chat-block-view-mode)
          (should-not e-chat-response-navigation-mode)
          (should (e-chat--point-in-composer-p))
          (should-not (eq (key-binding (kbd "h") t)
                          #'e-chat-block-view-left)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-add-context-clears-target-response-navigation-mode ()
  "Context insertion into a chat target exits stale response navigation state."
  (let ((buffer (e-chat-test--buffer nil "chat-context-response-nav"))
        (reference '(:uri "buffer://source"
                     :label "source:1"
                     :text "source"
                     :start-line 1
                     :end-line 1
                     :point-line 1)))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "one")
          (call-interactively #'e-chat-enter-response-navigation)
          (should e-chat-response-navigation-mode)
          (e-chat--add-context-reference-to-session
           reference
           e-chat-harness
           e-chat-session-id)
          (should-not e-chat-response-navigation-mode)
          (should-not e-chat-block-view-mode)
          (should (e-chat--point-in-composer-p))
          (should-not (eq (key-binding (kbd "j") t)
                          #'e-chat-response-navigation-next)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-add-context-picker-can-create-new-session ()
  "Picker context insertion can create a new session target."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store))))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-session-create store :id "existing-session")
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (car (all-completions "" collection)))))
              (with-temp-buffer
                (insert "one\ntwo\nthree\n")
                (goto-char (point-min))
                (let ((chat-buffer (e-chat-add-context-to-session)))
                  (with-current-buffer chat-buffer
                    (should-not (equal e-chat-session-id
                                       "existing-session"))
                    (should (= (length (e-harness-session-list
                                        e-chat-harness))
                               2))
                    (should (string-match-p
                             "@\\[.*:1-3\\]"
                             (buffer-substring-no-properties
                              e-chat--composer-start-marker
                              (point-max))))))))))
      (e-chat-test--kill-chat-buffers))))

(ert-deftest e-chat-test-add-context-picker-can-select-existing-session ()
  "Picker context insertion can target an existing chat session."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store))))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-session-create store :id "target-session"
                              :metadata '(:name "target-session"))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (cadr (all-completions "" collection)))))
              (with-temp-buffer
                (insert "one\ntwo\nthree\n")
                (goto-char (point-min))
                (let ((chat-buffer (e-chat-add-context-to-session)))
                  (with-current-buffer chat-buffer
                    (should (equal e-chat-session-id "target-session"))
                    (should (= (length (e-harness-session-list
                                        e-chat-harness))
                               1))
                    (should (string-match-p
                             "@\\[.*:1-3\\]"
                            (buffer-substring-no-properties
                             e-chat--composer-start-marker
                             (point-max))))))))))
      (e-chat-test--kill-chat-buffers))))

(ert-deftest e-chat-test-add-context-picker-preserves-session-list-order ()
  "Picker context insertion keeps store recency order under sorting frontends."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store)))
         (timestamps '("2026-05-22T10:00:00Z"
                       "2026-05-22T10:00:01Z"
                       "2026-05-22T10:00:02Z"
                       "2026-05-22T10:00:03Z")))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (cl-letf (((symbol-function 'e-session--timestamp)
                       (lambda (&optional _time)
                         (prog1 (car timestamps)
                           (setq timestamps (cdr timestamps))))))
              (e-session-create store :id "older-session"
                                :metadata '(:name "Alpha old"))
              (e-session-append-message
               store "older-session"
               '(:id "older-message" :role user :content "older prompt"))
              (e-session-create store :id "newer-session"
                                :metadata '(:name "Zulu newest"))
              (e-session-append-message
               store "newer-session"
               '(:id "newer-message" :role user :content "newer prompt")))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (let* ((metadata (completion-metadata
                                           "" collection nil))
                                (display-sort
                                 (completion-metadata-get
                                  metadata 'display-sort-function))
                                (candidates (all-completions "" collection))
                                (visible (if display-sort
                                             (funcall display-sort candidates)
                                           (sort (copy-sequence candidates)
                                                 #'string<))))
                           (cl-find-if
                            (lambda (candidate)
                              (not (equal candidate
                                          e-chat--new-context-session-label)))
                            visible)))))
              (with-temp-buffer
                (insert "one\ntwo\nthree\n")
                (goto-char (point-min))
                (let ((chat-buffer (e-chat-add-context-to-session)))
                  (with-current-buffer chat-buffer
                    (should (equal e-chat-session-id "newer-session"))))))))
      (e-chat-test--kill-chat-buffers))))

(ert-deftest e-chat-test-add-context-to-session-deactivates-source-region ()
  "Picker context insertion clears the selected region in the source buffer."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-chat-test--activate-chat-session
                   (e-harness-create :backend backend :sessions store))))
    (unwind-protect
        (e-chat-test--with-empty-harness-registry
          (let ((e-chat-default-harness-id :chat-test))
            (e-harness-registry-register :chat-test harness)
            (e-session-create store :id "target-session"
                              :metadata '(:name "target-session"))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (cadr (all-completions "" collection)))))
              (with-temp-buffer
                (insert "alpha beta gamma")
                (goto-char (point-min))
                (search-forward "beta")
                (set-mark (match-beginning 0))
                (setq mark-active t)
                (let ((chat-buffer (e-chat-add-context-to-session)))
                  (should-not mark-active)
                  (with-current-buffer chat-buffer
                    (should (equal e-chat-session-id "target-session"))
                    (should (string-match-p
                             "@\\[.*:1\\]"
                             (buffer-substring-no-properties
                              e-chat--composer-start-marker
                              (point-max))))))))))
      (e-chat-test--kill-chat-buffers))))

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

(ert-deftest e-chat-test-derived-title-updates-attached-buffer-display ()
  "Derived session titles refresh attached presentation surfaces."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer (e-chat-open :harness harness :session-id "derived-title"))
         (prompt "Derived title update"))
    (unwind-protect
        (progn
          (e-harness--append-user-message
           harness
           "derived-title"
           "turn-1"
           prompt)
          (e-harness--emit-turn-event
           harness
           "derived-title"
           "turn-1"
           'turn-finished
           nil)
          (with-current-buffer buffer
            (let ((title (e-session-display-title store "derived-title"))
                  (text (buffer-substring-no-properties
                         (point-min)
                         (min (point-max) 160))))
              (should (equal title prompt))
              (should (string-match-p (regexp-quote title) (buffer-name)))
              (should (string-match-p (regexp-quote title)
                                      header-line-format))
              (should (string-match-p (regexp-quote title) text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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

(ert-deftest e-chat-test-mode-line-status-formats-model-effort-and-context ()
  "Mode-line status includes model, effort, and estimated context usage."
  (should (equal (e-chat--model-context-token-limit "gpt-5.5")
                 258400))
  (should
   (equal
    (e-chat--format-mode-line-status "gpt-5.5" "high" 18000 400000 t)
    "e-chat gpt-5.5/high ~5% (~18k/400k tok)"))
  (should
   (equal
    (e-chat--format-mode-line-status "gpt-5.5" "high" 40000 258400 nil)
    "e-chat gpt-5.5/high 15% (40k/258k tok)")))

(ert-deftest e-chat-test-mode-line-status-uses-session-context ()
  "Attached chat buffers show session model, effort, and token context."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create
                   :backend backend
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (e-chat-model-context-token-limits '(("gpt-5.5" . 100)))
         (e-chat-context-token-estimate-bytes-per-token 1.0)
         (buffer (e-chat-open :harness harness :session-id "chat-mode-line")))
    (unwind-protect
        (with-current-buffer buffer
          (e-session-append-message
           store
           e-chat-session-id
           '(:role user :content "context question"))
          (e-chat--set-status "idle")
          (should (string-match-p "gpt-5.5/high" mode-name))
          (should (string-match-p "~[0-9]+%" mode-name))
          (should (string-match-p "/100 tok" mode-name)))
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
          (e-chat--set-status "idle")
          (should (equal mode-name
                         "e-chat gpt-5.5/high 78% (203k/258k tok)")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-token-usage-before-compaction-uses-context-estimate ()
  "After compaction, stale provider usage does not hide compacted context size."
  (let* ((timestamps '("2026-05-25T10:00:00Z"
                       "2026-05-25T10:00:01Z"
                       "2026-05-25T10:00:02Z"
                       "2026-05-25T10:00:03Z"
                       "2026-05-25T10:00:04Z"))
         (store (e-session-store-create))
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
      (cl-letf (((symbol-function 'e-session--timestamp)
                 (lambda (&optional _time)
                   (prog1 (car timestamps)
                     (setq timestamps (cdr timestamps))))))
        (e-session-create store :id e-chat-session-id)
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
         :first-kept-entry-id "kept"))
      (e-chat--set-status "idle")
      (should (string-match-p "~[0-9]+%" mode-name))
      (should-not (string-match-p "203k/258k tok" mode-name)))))

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
          (e-chat--render-event
           (e-events-make :type 'token-usage
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:input-tokens 54581
                                     :cached-input-tokens 30720
                                     :output-tokens 154
                                     :reasoning-output-tokens 0
                                     :total-tokens 54735)))
          (should (equal mode-name
                         "e-chat gpt-5.5/high 21% (55k/258k tok)"))
          (should-not (string-match-p "token-usage" (buffer-string)))
          (should-not (string-match-p "Event:" (buffer-string))))
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

(ert-deftest e-chat-test-composer-meta-actions-target-latest-response ()
  "Composer M-y and M-o target the latest final assistant response block."
  (unless (fboundp 'markdown-mode)
    (define-derived-mode markdown-mode text-mode "Markdown"))
  (let ((buffer (e-chat-test--buffer nil "chat-meta-actions"))
        (opened nil))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (e-chat-test--render-turn "turn-1" 10 11 "first" "old final")
          (e-chat-test--render-turn "turn-2" 20 21 "second" "latest final")
          (goto-char e-chat--composer-start-marker)
          (call-interactively
           (lookup-key e-chat-mode-map (kbd "M-y")))
          (should (equal (current-kill 0) "latest final"))
          (setq opened
                (call-interactively
                 (lookup-key e-chat-mode-map (kbd "M-o"))))
          (should (eq (window-buffer (selected-window)) opened))
          (with-current-buffer opened
            (should (derived-mode-p 'markdown-mode))
            (should (equal (buffer-string) "latest final"))))
      (when (buffer-live-p opened)
        (kill-buffer opened))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-keymap-refresh-updates-stale-reload-bindings ()
  "Reload-time keymap refresh replaces bindings preserved by `defvar'."
  (let ((e-chat-response-navigation-mode-map (make-sparse-keymap))
        (e-chat-mode-map (make-sparse-keymap))
        (e-chat-block-view-mode-map (make-sparse-keymap))
        (e-chat-tool-list-mode-map (make-sparse-keymap))
        (e-chat-tool-output-mode-map (make-sparse-keymap)))
    (define-key e-chat-response-navigation-mode-map
                (kbd "RET")
                'e-chat-response-navigation-expand)
    (e-chat--refresh-keymaps)
    (should (eq (lookup-key e-chat-response-navigation-mode-map (kbd "RET"))
                'e-chat-response-navigation-activate))
    (should (eq (lookup-key e-chat-response-navigation-mode-map (kbd "y"))
                'e-chat-response-navigation-copy))
    (should (eq (lookup-key e-chat-block-view-mode-map (kbd "G"))
                'e-chat-block-view-end))
    (should (eq (lookup-key e-chat-block-view-mode-map (kbd "g g"))
                'e-chat-block-view-beginning))
    (should (eq (lookup-key e-chat-block-view-mode-map (kbd "v"))
                'e-chat-block-view-select))
    (should (eq (lookup-key e-chat-block-view-mode-map (kbd "y"))
                'e-chat-block-view-copy))
    (should (eq (lookup-key e-chat-mode-map (kbd "M-y"))
                'e-chat-copy-latest-response))
    (should (eq (lookup-key e-chat-mode-map (kbd "M-o"))
                'e-chat-open-latest-response))))

(ert-deftest e-chat-test-context-mode-binds-default-reference-shortcuts ()
  "The global context keymap binds latest and picker insertion shortcuts."
  (should (eq (lookup-key e-chat-context-mode-map (kbd "s-i"))
              'e-chat-add-context-to-latest))
  (should (eq (lookup-key e-chat-context-mode-map (kbd "s-I"))
              'e-chat-add-context-to-session)))

(ert-deftest e-chat-test-evil-normal-context-bindings-use-super-i ()
  "Evil normal bindings use s-i and s-I without taking over bare I."
  (let (calls)
    (cl-letf (((symbol-function 'evil-define-key*)
               (lambda (&rest args)
                 (push args calls))))
      (e-chat--configure-evil-context-bindings))
    (should (member (list 'normal
                          e-chat-context-mode-map
                          (kbd "s-i")
                          #'e-chat-add-context-to-latest)
                    calls))
    (should (member (list 'normal
                          e-chat-context-mode-map
                          (kbd "s-I")
                          #'e-chat-add-context-to-session)
                    calls))
    (should-not (seq-some (lambda (call)
                            (equal (nth 2 call) (kbd "I")))
                          calls))))

(ert-deftest e-chat-test-keymap-preserves-host-alt-leader ()
  "The chat mode keymap preserves a host-provided alternate leader prefix."
  (let ((e-chat-mode-map (make-sparse-keymap))
        (had-alt-key (boundp 'doom-leader-alt-key))
        (old-alt-key (and (boundp 'doom-leader-alt-key)
                          (symbol-value 'doom-leader-alt-key)))
        (had-leader-map (boundp 'doom-leader-map))
        (old-leader-map (and (boundp 'doom-leader-map)
                             (symbol-value 'doom-leader-map)))
        (leader-map (make-sparse-keymap)))
    (unwind-protect
        (progn
          (define-key leader-map (kbd "f") #'find-file)
          (set 'doom-leader-alt-key "M-SPC")
          (set 'doom-leader-map leader-map)
          (e-chat--refresh-keymaps)
          (should (eq (lookup-key e-chat-mode-map (kbd "M-SPC"))
                      leader-map))
          (should (eq (lookup-key e-chat-mode-map (kbd "M-SPC f"))
                      #'find-file)))
      (if had-alt-key
          (set 'doom-leader-alt-key old-alt-key)
        (makunbound 'doom-leader-alt-key))
      (if had-leader-map
          (set 'doom-leader-map old-leader-map)
        (makunbound 'doom-leader-map)))))

(ert-deftest e-chat-test-shell-descriptor-advertises-chat-surface ()
  "The chat presentation publishes a generic shell manifest."
  (let* ((shell (e-chat-shell))
         (command-ids (mapcar #'e-shell-command-id
                              (e-shell-commands shell)))
         (keymaps (e-shell-keymaps shell)))
    (should (eq (e-shell-id shell) 'chat))
    (should (equal (e-shell-required-capabilities shell) '(chat-session)))
    (dolist (command-id '(new
                          resume
                          switch-session
                          overview
                          overview-close
                          rename
                          set-model
                          set-effort
                          show-context
                          submit
                          abort
                          reset
                          enter-response-navigation
                          response-navigation-next
                          response-navigation-previous
                          response-navigation-activate
                          response-navigation-copy
                          response-navigation-open
                          response-navigation-details
                          response-navigation-insert
                          open-latest-response
                          copy-latest-response
                          add-context-to-latest
                          add-context-to-session))
      (should (memq command-id command-ids)))
    (should (eq (plist-get (car keymaps) :keymap) e-chat-mode-map))
    (let ((context-keymap (cl-find 'context keymaps
                                   :key (lambda (entry)
                                          (plist-get entry :id)))))
      (should (eq (plist-get context-keymap :keymap) e-chat-context-mode-map))
      (should (eq (plist-get context-keymap :scope) 'global))
      (should (eq (plist-get context-keymap :mode) 'e-chat-context-mode)))
    (should (eq (plist-get (cl-find 'response-navigation keymaps
                                    :key (lambda (entry)
                                           (plist-get entry :id)))
                           :keymap)
                e-chat-response-navigation-mode-map))))

(ert-deftest e-chat-test-startup-enables-global-context-mode ()
  "Chat shell startup enables the global context insertion keymap."
  (let ((e-chat-context-mode nil))
    (e-chat-startup)
    (should e-chat-context-mode)))

(ert-deftest e-chat-test-registers-chat-shell-on-load ()
  "Loading e-chat registers the chat shell manifest."
  (should (eq (e-shell-id (e-shell-get 'chat)) 'chat))
  (should (eq (e-shell-command-interactive
               (e-shell-command-by-id (e-shell-get 'chat) 'new))
              'e-chat-new)))

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
            (should (equal (plist-get
                            (car (e-session-compactions
                                  store e-chat-session-id))
                            :summary)
                           "Compacted summary."))
            (should (string-match-p "Context compaction started"
                                    (buffer-string)))
            (should (string-match-p "Context compacted into"
                                    (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-chat-test)

;;; e-chat-test.el ends here

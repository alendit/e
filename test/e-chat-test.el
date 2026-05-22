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
  "Submitting shows the user turn and keeps bottom separator chrome visible."
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
          (should (string-match-p (regexp-quote e-chat--composer-separator)
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

(ert-deftest e-chat-test-post-command-keeps-point-inside-composer ()
  "Cursor commands cannot leave an active composer in normal edit mode."
  (let ((buffer (e-chat-test--buffer nil "chat-composer-post-command")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (run-hooks 'post-command-hook)
          (should (>= (point) (marker-position e-chat--composer-start-marker)))
          (should-not (get-text-property (point) 'e-chat-protected)))
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
                      ((symbol-function 'pop-to-buffer)
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
                             " ◐")
                     content))
            (goto-char (point-min))
            (search-forward "◐")
            (should (get-text-property (1- (point)) 'read-only)))
          (e-chat--advance-progress-indicator)
          (should (string-match-p
                   (concat (regexp-quote e-chat--assistant-glyph)
                           " ◓")
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
                                 " [◐◓◑◒]")
                         content))))
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

(ert-deftest e-chat-test-keeps-activity-timeline-with-final-response ()
  "Intermittent text remains visible and tool details expand after final output."
  (let ((buffer (e-chat-test--buffer nil "chat-intermittent")))
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
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type tool-call
                                      :id "call-1"
                                      :name "buffer-read"
                                      :arguments (:buffer "*scratch*"))))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:message
                                     (:role tool-call
                                      :content
                                      (:type tool-call
                                       :id "call-1"
                                       :name "buffer-read"
                                       :arguments (:buffer "*scratch*"))))))
          (e-chat--render-event
           (e-events-make :type 'tool-finished
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:tool-call
                                     (:type tool-call
                                      :id "call-1"
                                      :name "buffer-read"
                                     :arguments (:buffer "*scratch*"))
                                     :result (:status ok
                                              :content "scratch contents"))))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10
                          :payload '(:message
                                     (:role tool
                                      :content (:status ok
                                                :content "scratch contents")))))
          (let ((content (buffer-string)))
            (should (string-match-p "Need current buffer state\\." content))
            (should (string-match-p "Need current buffer state\\.\n1 tool call"
                                    content))
            (should-not (string-match-p "Working" content))
            (should-not (string-match-p "buffer-read" content))
            (should-not (string-match-p "\\*scratch\\*" content))
            (should-not (string-match-p "scratch contents" content))
            (should-not (string-match-p "Reasoning" content))
            (let ((case-fold-search nil))
              (should-not (string-match-p "Tool call\n" content))))
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
            (should (string-match-p "Need current buffer state\\." content))
            (should (string-match-p
                     "Need current buffer state\\.\n1 tool call"
                     content))
            (should-not (string-match-p "Worked for" content))
            (should-not (string-match-p "buffer-read" content))
            (should-not (string-match-p "\\*scratch\\*" content))
            (should-not (string-match-p "scratch contents" content))
            (goto-char (point-min))
            (search-forward "1 tool call")
            (call-interactively #'e-chat-enter-response-navigation)
            (should (eq (plist-get (e-chat-test--focused-block) :kind)
                        'activity))
            (call-interactively #'e-chat-response-navigation-activate)
            (should e-chat-tool-list-mode)
            (let ((tool-list (buffer-string)))
              (should (string-match-p "buffer-read" tool-list))
              (should (string-match-p "\\*scratch\\*" tool-list))
              (should-not (string-match-p "scratch contents" tool-list)))
            (let ((output (e-chat-tool-list-open-output)))
              (with-current-buffer output
                (should (derived-mode-p 'e-chat-tool-output-mode))
                (should (string-match-p "scratch contents" (buffer-string)))
                (call-interactively #'e-chat-tool-output-back)))
            (with-current-buffer buffer
              (should e-chat-tool-list-mode)
              (call-interactively #'e-chat-tool-list-back)
              (should e-chat-response-navigation-mode)
              (goto-char (point-min))
              (should (= (how-many "buffer-read") 0))
              (goto-char (point-min))
              (should (= (how-many "scratch contents") 0)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-activity-groups-tool-counts-after-reasoning ()
  "Collapsed activity shows tool counts after the reasoning they followed."
  (let ((buffer (e-chat-test--buffer nil "chat-activity-groups")))
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
                          :payload '(:content "reasoning 1")))
          (dotimes (index 2)
            (e-chat--render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload (list :type 'tool-call
                                           :id (format "call-a-%d" index)
                                           :name "read"))))
          (e-chat--render-event
           (e-events-make :type 'reasoning-delta
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:content "reasoning 2")))
          (dotimes (index 4)
            (e-chat--render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload (list :type 'tool-call
                                           :id (format "call-b-%d" index)
                                           :name "read"))))
          (let ((content (buffer-string)))
            (should (string-match-p
                     "reasoning 1\n2 tool calls\n\nreasoning 2\n4 tool calls"
                     content))
            (should-not (string-match-p "6 tool calls" content))
            (should-not (string-match-p "Working" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-progress-and-tool-count-order-stays-stable ()
  "Progress ticks do not reorder the running activity summary."
  (let ((buffer (e-chat-test--buffer nil "chat-running-status-order")))
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
          (should (string-match-p
                   (concat (regexp-quote e-chat--assistant-glyph)
                           " ◐\n\n1 tool call")
                   (buffer-string)))
          (e-chat--advance-progress-indicator)
          (should (string-match-p
                   (concat (regexp-quote e-chat--assistant-glyph)
                           " ◓\n\n1 tool call")
                   (buffer-string))))
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

(ert-deftest e-chat-test-response-navigation-details-open-read-only-buffer ()
  "Details command opens turn metadata and tool details outside the chat buffer."
  (let ((buffer (e-chat-test--buffer nil "chat-nav-details")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "inspect" "done")
          (e-chat--record-tool-message
           "turn-1"
           '(:role tool-call
             :content (:type tool-call :name "buffer-read"
                       :arguments (:buffer "*scratch*"))))
          (e-chat--record-tool-message
           "turn-1"
           '(:role tool
             :content (:status ok :content "scratch contents")))
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

(ert-deftest e-chat-test-activity-tool-list-navigates-calls-and-output ()
  "Intermittent blocks open a call list; j/k move and RET opens output."
  (let ((buffer (e-chat-test--buffer nil "chat-tool-list"))
        (output nil))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat--render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (dolist (tool '(("call-1" "buffer-read" "first result")
                          ("call-2" "buffer-write" "second result")))
            (e-chat--render-event
             (e-events-make :type 'tool-started
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload (list :type 'tool-call
                                           :id (nth 0 tool)
                                           :name (nth 1 tool)
                                           :arguments '(:buffer "*scratch*"))))
            (e-chat--render-event
             (e-events-make :type 'tool-finished
                            :session-id e-chat-session-id
                            :turn-id "turn-1"
                            :payload (list :tool-call
                                           (list :type 'tool-call
                                                 :id (nth 0 tool)
                                                 :name (nth 1 tool)
                                                 :arguments '(:buffer "*scratch*"))
                                           :result (list :status 'ok
                                                         :content (nth 2 tool))))))
          (e-chat--render-event
           (e-events-make :type 'message-added
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 11
                          :payload '(:message (:role assistant
                                                :content "Final answer."))))
          (e-chat-test--focus-block-containing "2 tool calls")
          (call-interactively #'e-chat-response-navigation-activate)
          (should e-chat-tool-list-mode)
          (should (equal e-chat--tool-list-index 0))
          (call-interactively #'e-chat-tool-list-next)
          (should (equal e-chat--tool-list-index 1))
          (setq output (e-chat-tool-list-open-output))
          (with-current-buffer output
            (should (derived-mode-p 'e-chat-tool-output-mode))
            (should (string-match-p "second result" (buffer-string)))
            (call-interactively #'e-chat-tool-output-back))
          (should (eq (current-buffer) buffer))
          (should e-chat-tool-list-mode)
          (call-interactively #'e-chat-tool-list-back)
          (should e-chat-response-navigation-mode))
      (when (buffer-live-p output)
        (kill-buffer output))
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
                (should (string-match-p "  Duration: 2.00s"
                                        (buffer-string)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-buffer-name e-chat-details-buffer-name))))

(ert-deftest e-chat-test-replayed-session-hides-tool-messages-in-final-turn ()
  "Replayed tool messages stay out of the transcript and move to details."
  (let* ((store (e-session-store-create))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "chat-tool-replay")
          (e-session-append-message
           store "chat-tool-replay" '(:role user :content "inspect"))
          (e-session-append-message
           store "chat-tool-replay"
           '(:role tool-call
             :content (:type tool-call :name "run_elisp" :arguments (:form "(buffer-name)"))))
          (e-session-append-message
           store "chat-tool-replay"
           '(:role tool
             :content (:tool-call-id "call-1"
                       :name "run_elisp"
                       :status ok
                       :content (:result "*scratch*"))))
          (e-session-append-message
           store "chat-tool-replay" '(:role assistant :content "Final."))
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-tool-replay"))
          (with-current-buffer buffer
            (let ((content (buffer-string)))
              (should (string-match-p "Final\\." content))
              (should-not (string-match-p "· Tool" content))
              (should-not (string-match-p ":tool-call-id" content)))
            (call-interactively #'e-chat-enter-response-navigation)
            (let ((details (e-chat-response-navigation-details)))
              (with-current-buffer details
                (should (string-match-p "  Tool call\n  run_elisp"
                                        (buffer-string)))
                (should (string-match-p "  Tool\n  (:tool-call-id"
                                        (buffer-string)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-buffer-name e-chat-details-buffer-name))))

(ert-deftest e-chat-test-replayed-session-restores-activity-timeline ()
  "Replayed durable activity events render before the final answer."
  (let* ((directory (make-temp-file "e-chat-activity-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "chat-activity-replay")
          (e-session-append-message
           store "chat-activity-replay"
           '(:role user :content "inspect" :turn-id "turn-1"))
          (e-session-append-activity-event
           store "chat-activity-replay" "turn-1" 'turn-started nil)
          (e-session-append-activity-event
           store "chat-activity-replay" "turn-1" 'reasoning-delta
           '(:content "Need current buffer state."))
          (e-session-append-activity-event
           store "chat-activity-replay" "turn-1" 'tool-started
           '(:type tool-call
             :id "call-1"
             :name "buffer-read"
             :arguments (:buffer "*scratch*")))
          (e-session-append-activity-event
           store "chat-activity-replay" "turn-1" 'tool-finished
           '(:tool-call
             (:type tool-call
              :id "call-1"
              :name "buffer-read"
              :arguments (:buffer "*scratch*"))
             :result (:status ok :content "scratch contents")))
          (e-session-append-message
           store "chat-activity-replay"
           '(:role assistant :content "Final answer." :turn-id "turn-1"))
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-activity-replay"))
          (with-current-buffer buffer
            (let ((content (buffer-string)))
              (should (string-match-p "Need current buffer state\\." content))
              (should (string-match-p "Need current buffer state\\.\n1 tool call"
                                      content))
              (should-not (string-match-p "Worked for" content))
              (should-not (string-match-p "buffer-read" content))
              (should (string-match-p "Final answer\\." content))
              (should-not (string-match-p "scratch contents" content)))
            (goto-char (point-min))
            (search-forward "Final answer.")
            (call-interactively #'e-chat-enter-response-navigation)
            (let ((details (e-chat-response-navigation-details)))
              (with-current-buffer details
                (should (string-match-p "scratch contents"
                                        (buffer-string)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-test--kill-buffer-name e-chat-details-buffer-name)
      (delete-directory directory t))))

(ert-deftest e-chat-test-replayed-activity-keeps-earlier-turns-visible ()
  "Replayed activity summaries do not erase earlier transcript blocks."
  (let* ((directory (make-temp-file "e-chat-activity-history-" t))
         (store (e-session-persistent-store-create directory))
         (backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend :sessions store))
         (buffer nil))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "chat-activity-history")
          (e-session-append-message
           store "chat-activity-history"
           '(:role user :content "first prompt" :turn-id "turn-1"))
          (e-session-append-activity-event
           store "chat-activity-history" "turn-1" 'reasoning-delta
           '(:content "First reasoning."))
          (e-session-append-message
           store "chat-activity-history"
           '(:role assistant :content "First final." :turn-id "turn-1"))
          (e-session-append-message
           store "chat-activity-history"
           '(:role user :content "second prompt" :turn-id "turn-2"))
          (e-session-append-activity-event
           store "chat-activity-history" "turn-2" 'reasoning-delta
           '(:content "Second reasoning."))
          (e-session-append-message
           store "chat-activity-history"
           '(:role assistant :content "Second final." :turn-id "turn-2"))
          (setq buffer (e-chat-open :harness harness
                                    :session-id "chat-activity-history"))
          (with-current-buffer buffer
            (let ((content (buffer-string)))
              (should (string-match-p "first prompt" content))
              (should (string-match-p "First reasoning\\." content))
              (should (string-match-p "First final\\." content))
              (should (string-match-p "second prompt" content))
              (should (string-match-p "Second reasoning\\." content))
              (should (string-match-p "Second final\\." content)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

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

(ert-deftest e-chat-test-add-context-to-latest-targets-most-recent-session ()
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
                         (car collection))))
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
                         (cadr collection))))
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

(ert-deftest e-chat-test-composer-meta-actions-target-latest-response ()
  "Composer M-y and M-o target the latest final assistant response block."
  (let ((buffer (e-chat-test--buffer nil "chat-meta-actions"))
        (opened nil))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-test--render-turn "turn-1" 10 11 "first" "old final")
          (e-chat-test--render-turn "turn-2" 20 21 "second" "latest final")
          (goto-char e-chat--composer-start-marker)
          (call-interactively
           (lookup-key e-chat-mode-map (kbd "M-y")))
          (should (equal (current-kill 0) "latest final"))
          (setq opened
                (call-interactively
                 (lookup-key e-chat-mode-map (kbd "M-o"))))
          (with-current-buffer opened
            (should (derived-mode-p 'text-mode))
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
  "The opt-in context keymap binds latest and picker insertion shortcuts."
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
    (should (eq (plist-get (cadr keymaps) :keymap)
                e-chat-response-navigation-mode-map))))

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

(provide 'e-chat-test)

;;; e-chat-test.el ends here

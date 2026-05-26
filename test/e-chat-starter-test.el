;;; e-chat-starter-test.el --- Tests for global chat starter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the global one-shot chat starter presentation shell.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-chat)
(require 'e-chat-session)
(require 'e-chat-starter)
(require 'e-harness)
(require 'e-shells)

(defun e-chat-starter-test--harness (&optional items)
  "Return a test harness backed by fake backend ITEMS."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items items))))
    (e-harness-activate-capability
     harness
     (e-chat-session-capability-create))
    harness))

(defun e-chat-starter-test--answered-state (buffer-name)
  "Return an answered starter state wired to BUFFER-NAME."
  (let* ((harness (e-chat-starter-test--harness))
         (subscription (e-harness-subscribe harness (lambda (_event))))
         (buffer (get-buffer-create buffer-name))
         (state (make-e-chat-starter-state
                 :harness harness
                 :session-id "starter-action"
                 :question "Why?"
                 :source-reference '(:label "demo.el:4"
                                     :uri "file:///tmp/demo.el")
                 :status 'answered
                 :latest-answer "Because."
                 :subscription subscription)))
    (with-current-buffer buffer
      (e-chat-starter-mode)
      (setq-local e-chat-starter--state state))
    (setf (e-chat-starter-state-buffer state) buffer)
    state))

(ert-deftest e-chat-starter-test-captures-region-source-reference ()
  "Source capture records exact active region text and line bounds."
  (with-temp-buffer
    (rename-buffer "starter-source" t)
    (insert "alpha\nbeta\ngamma\n")
    (goto-char (point-min))
    (forward-line 1)
    (set-mark (point))
    (end-of-line)
    (setq mark-active t)
    (let ((reference (e-chat-starter--capture-source)))
      (should (equal (plist-get reference :uri) "buffer://starter-source"))
      (should (equal (plist-get reference :text) "beta"))
      (should (equal (plist-get reference :start-line) 2))
      (should (equal (plist-get reference :end-line) 2))
      (should (equal (plist-get reference :point-line) 2))
      (should (string-match-p "starter-source:2"
                              (plist-get reference :label))))))

(ert-deftest e-chat-starter-test-captures-bounded-point-context ()
  "Source capture around point is bounded by the configured line radius."
  (let ((e-chat-starter-context-line-radius 1))
    (with-temp-buffer
      (rename-buffer "starter-window" t)
      (insert "one\ntwo\nthree\nfour\nfive\n")
      (goto-char (point-min))
      (forward-line 2)
      (let ((reference (e-chat-starter--capture-source)))
        (should (equal (plist-get reference :start-line) 2))
        (should (equal (plist-get reference :end-line) 4))
        (should (equal (plist-get reference :text) "two\nthree\nfour\n"))))))

(ert-deftest e-chat-starter-test-format-prompt-includes-reference-section ()
  "Starter prompt embeds a placeholder and chat-compatible reference section."
  (let* ((reference '(:id "ref-1"
                     :label "demo.el:4"
                     :uri "file:///tmp/demo.el"
                     :text "(message \"hi\")"))
         (prompt (e-chat-starter--format-prompt "Explain this" reference)))
    (should (string-match-p "Explain this" prompt))
    (should (string-match-p "<reference id=\"ref-1\" label=\"demo.el:4\">"
                            prompt))
    (should (string-match-p "References:" prompt))
    (should (string-match-p "\\[ref-1\\] demo.el:4 (file:///tmp/demo.el)"
                            prompt))
    (should (string-match-p "(message \"hi\")" prompt))))

(ert-deftest e-chat-starter-test-render-answered-popup-and-keymap ()
  "Answered popup uses chat-style blocks and starter action keys."
  (let ((state (make-e-chat-starter-state
                :question "What does this do?"
                :source-reference '(:label "demo.el:4")
                :status 'answered
                :latest-answer "It explains the form.")))
    (with-current-buffer (get-buffer-create "*e-chat-starter-render-test*")
      (unwind-protect
          (progn
            (e-chat-starter-mode)
            (setq-local e-chat-starter--state state)
            (e-chat-starter--render)
            (let ((text (buffer-string)))
              (should (string-match-p "What does this do\\?" text))
              (should (string-match-p "It explains the form\\." text))
              (should (string-match-p "\\[c\\] continue" text))
              (should-not (string-match-p "Question:" text))
              (should-not (string-match-p "Response:" text)))
            (goto-char (point-min))
            (search-forward "What does this do?")
            (should (eq (get-text-property (match-beginning 0)
                                           'font-lock-face)
                        'e-chat-user-face))
            (search-forward "It explains the form.")
            (should (memq 'e-chat-final-assistant-face
                          (ensure-list
                           (get-text-property (match-beginning 0) 'face))))
            (should (eq (lookup-key e-chat-starter-mode-map (kbd "c"))
                        #'e-chat-starter-continue))
            (should (eq (lookup-key e-chat-starter-mode-map (kbd "o"))
                        #'e-chat-starter-open-answer))
            (should (eq (lookup-key e-chat-starter-mode-map (kbd "y"))
                        #'e-chat-starter-copy-answer))
            (should (eq (lookup-key e-chat-starter-mode-map (kbd "q"))
                        #'e-chat-starter-dismiss)))
        (kill-buffer (current-buffer))))))

(ert-deftest e-chat-starter-test-mode-neutralizes-evil-normal-state ()
  "Starter mode keeps Evil normal bindings from shadowing starter actions."
  (let (evil-local-mode-argument)
    (with-current-buffer (get-buffer-create "*e-chat-starter-evil-test*")
      (unwind-protect
          (cl-letf (((symbol-function 'evil-local-mode)
                     (lambda (argument)
                       (setq evil-local-mode-argument argument)
                       (setq-local evil-local-mode
                                   (not (and (numberp argument)
                                             (< argument 0))))
                       (unless evil-local-mode
                         (setq-local evil-state nil)))))
            (e-chat-starter-mode)
            (should (equal evil-local-mode-argument -1))
            (should-not evil-local-mode)
            (should-not evil-state))
        (kill-buffer (current-buffer))))))

(ert-deftest e-chat-starter-test-answered-popup-renders-markdown ()
  "Answered popup applies assistant Markdown presentation in the sidebar."
  (let ((state (make-e-chat-starter-state
                :question "Where is the review?"
                :source-reference '(:label "index.org:20")
                :status 'answered
                :latest-answer "Use `docs/feats/14-canvas-shell/review.org`.")))
    (with-current-buffer (get-buffer-create "*e-chat-starter-markdown-test*")
      (unwind-protect
          (progn
            (e-chat-starter-mode)
            (setq-local e-chat-starter--state state)
            (cl-letf (((symbol-function 'e-chat--apply-markdown-mode-properties)
                       (lambda (_start _end) nil)))
              (e-chat-starter--render))
            (goto-char (point-min))
            (search-forward "docs/feats/14-canvas-shell/review.org")
            (let ((faces (ensure-list
                          (get-text-property (1- (point)) 'face))))
              (should (memq 'e-chat-final-assistant-face faces))
              (should (memq 'e-chat-markdown-code-face faces))))
        (kill-buffer (current-buffer))))))

(ert-deftest e-chat-starter-test-events-record-first-final-answer ()
  "Starter state records assistant messages and ignores transient progress."
  (let ((state (make-e-chat-starter-state
                :status 'running
                :session-id "starter-session")))
    (e-chat-starter--handle-event
     state
     (e-events-make :type 'reasoning-delta
                    :session-id "starter-session"
                    :turn-id "turn-1"
                    :payload '(:content "thinking")))
    (should (equal (e-chat-starter-state-status state) 'running))
    (should (equal (e-chat-starter-state-latest-answer state) nil))
    (e-chat-starter--handle-event
     state
     (e-events-make :type 'message-added
                    :session-id "starter-session"
                    :turn-id "turn-1"
                    :payload '(:message
                               (:role assistant
                                :content "final answer"))))
    (should (equal (e-chat-starter-state-status state) 'answered))
    (should (equal (e-chat-starter-state-latest-answer state)
                   "final answer"))))

(ert-deftest e-chat-starter-test-start-creates-session-and-captures-answer ()
  "Starting here creates one real chat session and records the backend answer."
  (let* ((harness (e-chat-starter-test--harness
                   '((:type assistant-message :content "starter answer")
                     (:type done :reason stop)))))
    (with-temp-buffer
      (rename-buffer "starter-integration" t)
      (insert "target form\n")
      (let* ((state (e-chat-starter--start "Explain it"
                                           :harness harness
                                           :display nil))
             (session-id (e-chat-starter-state-session-id state)))
        (should session-id)
        (should (equal
                 (plist-get
                  (plist-get
                   (e-session-get (e-harness-sessions harness) session-id)
                   :metadata)
                  :origin)
                 :global-session-starter))
        (should (equal
                 (plist-get (e-harness-wait harness session-id 1.0) :status)
                 'done))
        (should (equal (e-chat-starter-state-latest-answer state)
                       "starter answer"))
        (should (equal (e-chat-starter-state-status state) 'answered))
        (when-let ((buffer (e-chat-starter-state-buffer state)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest e-chat-starter-test-continue-closes-popup ()
  "Continuing opens the chat session and closes the starter popup."
  (let* ((state (e-chat-starter-test--answered-state
                 "*e-chat-starter-continue*"))
         (harness (e-chat-starter-state-harness state))
         (subscription (e-chat-starter-state-subscription state))
         (popup (e-chat-starter-state-buffer state))
         opened)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'e-chat-open-session)
                     (lambda (open-harness open-session-id &optional display)
                       (setq opened (list open-harness open-session-id display))
                       :opened)))
            (should (eq (with-current-buffer popup
                          (call-interactively #'e-chat-starter-continue))
                        :opened)))
          (should (eq (car opened) harness))
          (should (equal (cadr opened) "starter-action"))
          (should-not (buffer-live-p popup))
          (should-not (memq subscription (e-harness-subscribers harness))))
      (when (buffer-live-p popup)
        (kill-buffer popup)))))

(ert-deftest e-chat-starter-test-open-answer-closes-popup ()
  "Opening the answer keeps the answer buffer and closes the starter popup."
  (let* ((state (e-chat-starter-test--answered-state
                 "*e-chat-starter-open-answer*"))
         (harness (e-chat-starter-state-harness state))
         (subscription (e-chat-starter-state-subscription state))
         (popup (e-chat-starter-state-buffer state))
         answer-buffer)
    (unwind-protect
        (progn
          (setq answer-buffer
                (with-current-buffer popup
                  (call-interactively #'e-chat-starter-open-answer)))
          (should-not (buffer-live-p popup))
          (should (buffer-live-p answer-buffer))
          (should-not (memq subscription (e-harness-subscribers harness)))
          (with-current-buffer answer-buffer
            (should (string-match-p "Because." (buffer-string)))
            (should (equal e-chat-starter-answer-session-id
                           "starter-action"))))
      (when (buffer-live-p answer-buffer)
        (kill-buffer answer-buffer))
      (when (buffer-live-p popup)
        (kill-buffer popup)))))

(ert-deftest e-chat-starter-test-copy-answer-closes-popup ()
  "Copying the answer stores it and closes the starter popup."
  (let* ((state (e-chat-starter-test--answered-state
                 "*e-chat-starter-copy-answer*"))
         (harness (e-chat-starter-state-harness state))
         (subscription (e-chat-starter-state-subscription state))
         (popup (e-chat-starter-state-buffer state)))
    (unwind-protect
        (progn
          (should (equal (with-current-buffer popup
                           (call-interactively #'e-chat-starter-copy-answer))
                         "Because."))
          (should (equal (current-kill 0 t) "Because."))
          (should-not (buffer-live-p popup))
          (should-not (memq subscription (e-harness-subscribers harness))))
      (when (buffer-live-p popup)
        (kill-buffer popup)))))

(ert-deftest e-chat-starter-test-shell-manifest-declares-chat-dependency ()
  "Starter shell is discoverable and declares its chat dependency."
  (let* ((shell (e-chat-starter-shell))
         (command (e-shell-command-by-id shell 'start-here)))
    (should (equal (e-shell-id shell) 'global-session-starter))
    (should (equal (plist-get (e-shell-metadata shell) :depends-on)
                   '(chat)))
    (should command)
    (should (eq (e-shell-command-interactive command)
                'e-chat-start-here))
    (should (eq (e-shell-command-function command)
                'e-chat-start-here))))

(provide 'e-chat-starter-test)

;;; e-chat-starter-test.el ends here

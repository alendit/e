;;; e-chat-test.el --- Tests for e chat presentation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the basic chat buffer presentation shell.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-chat)
(require 'e-harness)

(ert-deftest e-chat-test-open-submit-and-render-fake-response ()
  "The chat buffer submits a prompt through the harness and renders response."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "hello back")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness :session-id "chat-test")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-submit "hello")
          (e-harness-wait e-chat-harness e-chat-session-id 1.0)
          (let ((content (buffer-string)))
            (should (string-match-p "User: hello" content))
            (should (string-match-p "Assistant: hello back" content))
            (should (string-match-p "Turn finished" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-renders-empty-output-diagnostic ()
  "The chat buffer renders turns that finish without assistant output."
  (let* ((backend (e-backend-fake-create
                   :items '((:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness :session-id "chat-empty-output")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-submit "hello")
          (e-harness-wait e-chat-harness e-chat-session-id 1.0)
          (should (string-match-p "Backend returned no assistant output"
                                  (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-inline-prompt-is-editable-and-submits ()
  "The chat buffer prompt accepts typed text and submits it inline."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "inline answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness :session-id "chat-inline")))
    (unwind-protect
        (with-current-buffer buffer
          (should-not buffer-read-only)
          (goto-char (point-max))
          (insert "typed prompt")
          (e-chat-submit)
          (e-harness-wait e-chat-harness e-chat-session-id 1.0)
          (let ((content (buffer-string)))
            (should (string-match-p "User: typed prompt" content))
            (should (string-match-p "Assistant: inline answer" content))
            (should (string-match-p "\n> $" content))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-configures-evil-normal-state-when-available ()
  "The chat buffer configures Evil to treat it as a normal editable mode."
  (let (configured-mode configured-state)
    (cl-letf (((symbol-function 'evil-set-initial-state)
               (lambda (mode state)
                 (setq configured-mode mode)
                 (setq configured-state state))))
      (e-chat--configure-modal-state)
      (should (equal configured-mode 'e-chat-mode))
      (should (equal configured-state 'normal)))))

(ert-deftest e-chat-test-reload-normalizes-stale-special-mode-map ()
  "Reloading the chat mode drops stale special-mode keymap inheritance."
  (let ((original-map e-chat-mode-map)
        (stale-map (make-sparse-keymap)))
    (unwind-protect
        (progn
          (set-keymap-parent stale-map special-mode-map)
          (setq e-chat-mode-map stale-map)
          (e-chat--make-mode-map e-chat-mode-map)
          (should (eq (keymap-parent e-chat-mode-map) text-mode-map))
          (should-not (eq (lookup-key e-chat-mode-map "q") #'quit-window))
          (should (eq (lookup-key e-chat-mode-map (kbd "RET"))
                      #'e-chat-submit)))
      (setq e-chat-mode-map original-map))))

(ert-deftest e-chat-test-prompt-text-recovers-stale-marker ()
  "Prompt text is read from the visible prompt when marker state is stale."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness :session-id "chat-stale-marker")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "visible prompt")
          (set-marker e-chat--prompt-marker (point-max))
          (should (equal (e-chat--prompt-text) "visible prompt"))
          (should (equal (marker-position e-chat--prompt-marker)
                         (save-excursion
                           (goto-char (point-max))
                           (beginning-of-line)
                           (+ (line-beginning-position)
                              (length e-chat--prompt-marker-prefix))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-prompt-text-accepts-evil-insert-before-space ()
  "Prompt text works when modal insertion places text before the prompt space."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness :session-id "chat-evil-insert")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char e-chat--prompt-marker)
          (backward-char)
          (insert "hi")
          (should (equal (buffer-substring-no-properties
                          (line-beginning-position)
                          (point-max))
                         ">hi "))
          (should (equal (e-chat--prompt-text) "hi")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-open-does-not-force-evil-insert-state ()
  "The chat buffer leaves modal state changes to normal editor commands."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend))
         insert-called
         buffer)
    (cl-letf (((symbol-function 'evil-insert-state)
               (lambda ()
                 (setq insert-called t))))
      (unwind-protect
          (progn
            (setq buffer (e-chat-open :harness harness :session-id "chat-evil"))
            (should-not insert-called))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest e-chat-test-reload-buffers-refreshes-existing-chat-harness ()
  "Reloading chat buffers reattaches them to a fresh default harness."
  (let* ((old-backend (e-backend-fake-create :items nil))
         (old-harness (e-harness-create :backend old-backend))
         (new-backend (e-backend-fake-create
                       :items '((:type assistant-message :content "fresh answer")
                                (:type done :reason stop))))
         (new-harness (e-harness-create :backend new-backend))
         (buffer (e-chat-open :harness old-harness :session-id "chat-reload")))
    (unwind-protect
        (progn
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
            (e-chat-submit "hello")
            (e-harness-wait e-chat-harness e-chat-session-id 1.0)
            (should (string-match-p "Assistant: fresh answer"
                                    (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-test-reset-clears-rendered-session ()
  "Reset clears the rendered chat buffer and harness session transcript."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness :session-id "chat-reset")))
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-submit "question")
          (e-harness-wait e-chat-harness e-chat-session-id 1.0)
          (e-chat-reset)
          (should (string-match-p "Session reset" (buffer-string)))
          (should (equal (e-harness-messages e-chat-harness e-chat-session-id)
                         nil)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-chat-test)

;;; e-chat-test.el ends here

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

(ert-deftest e-chat-test-open-enters-evil-insert-state-when-available ()
  "The chat buffer enters Evil insert state when Evil is loaded."
  (let* ((backend (e-backend-fake-create :items nil))
         (harness (e-harness-create :backend backend))
         entered-buffer
         buffer)
    (cl-letf (((symbol-function 'evil-insert-state)
               (lambda ()
                 (setq entered-buffer (current-buffer)))))
      (unwind-protect
          (progn
            (setq buffer (e-chat-open :harness harness :session-id "chat-evil"))
            (should (eq entered-buffer buffer)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

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

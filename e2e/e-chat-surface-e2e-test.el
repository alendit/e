;;; e-chat-surface-e2e-test.el --- Chat surface E2E tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Deterministic UI E2E coverage for the interactive chat surface.  It uses
;; the fake backend but exercises actual Emacs buffers, windows, focus, and
;; the public submit command.

;;; Code:

(require 'ert)
(require 'e-backend)
(require 'e-chat)
(require 'e-harness)

(ert-deftest e-chat-surface-e2e-test-composer-submits-below-transcript ()
  "A displayed chat keeps input in its pane and responses in its transcript."
  (let* ((e-chat--surface-composition-enabled t)
         (e-chat-submit-backend-delay 0)
         (backend (e-backend-fake-create
                   :items '((:type assistant-message :content "surface answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (session-id "chat-surface-e2e")
         (buffer (e-chat-open :harness harness :session-id session-id))
         (window-configuration (current-window-configuration)))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (e-chat--after-display-buffer buffer)
          (with-current-buffer buffer
            (let* ((composer e-chat--surface-composer-buffer)
                   (transcript-window (get-buffer-window buffer t))
                   (composer-window (get-buffer-window composer t)))
              (should (window-live-p transcript-window))
              (should (window-live-p composer-window))
              (should (eq (window-buffer composer-window) composer))
              (should (> (nth 1 (window-edges composer-window))
                         (nth 1 (window-edges transcript-window))))
              (with-current-buffer composer
                (goto-char (point-max))
                (insert "surface prompt")
                (e-chat-submit))
              (should (equal (plist-get
                              (e-harness-wait-batch harness session-id 1.0)
                              :status)
                             'done))
              (should (string-match-p "surface prompt" (buffer-string)))
              (should (string-match-p "surface answer" (buffer-string)))
              (should-not (string-match-p (regexp-quote e-chat--composer-glyph)
                                          (buffer-string)))
              (with-current-buffer composer
                (should (equal (e-chat--composer-text) ""))
                (e-chat-composer-enter-navigation))
              (should (eq (selected-window) transcript-window)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (set-window-configuration window-configuration))))

(provide 'e-chat-surface-e2e-test)

;;; e-chat-surface-e2e-test.el ends here

;;; e-chat-surface-e2e-test.el --- Chat surface E2E tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Deterministic UI E2E coverage for the interactive chat surface.  It uses
;; the fake backend but exercises actual Emacs buffers, windows, focus, and
;; the public submit command.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-backend)
(require 'e-board-runtime)
(require 'e-chat)
(require 'e-chat-session)
(require 'e-harness)
(require 'e-harness-registry)
(require 'e-task-queue)
(load (expand-file-name
       "e-board-e2e-support.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil nil t)

(declare-function e-board-e2e-reset-runtime "e-board-e2e-support")
(declare-function e-board-e2e-drain-session "e-board-e2e-support")
(declare-function e-board-e2e-wait-batch "e-board-e2e-support")
(declare-function e-board-e2e-wait-until "e-board-e2e-support")

(ert-deftest e-chat-surface-e2e-test-composer-submits-below-transcript ()
  "A displayed chat keeps input in its pane and responses in its transcript."
  (e-board-e2e-reset-runtime)
  (let* ((e-chat--surface-composition-enabled t)
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
                   (pair (cl-find-if
                          (lambda (candidate)
                            (and (window-live-p (car candidate))
                                 (window-live-p (cdr candidate))
                                 (eq (window-buffer (car candidate)) buffer)
                                 (eq (window-buffer (cdr candidate)) composer)))
                          e-chat--surface-window-pairs))
                   (transcript-window (car pair))
                   (composer-window (cdr pair)))
              (should (window-live-p transcript-window))
              (should (window-live-p composer-window))
              (should (eq (window-buffer composer-window) composer))
              (should (> (nth 1 (window-edges composer-window))
                         (nth 1 (window-edges transcript-window))))
              (should (= (window-body-height composer-window) 4))
              (should buffer-read-only)
              (with-current-buffer composer
                (should-not (string-match-p
                             (regexp-quote e-chat--composer-separator)
                             (buffer-string))))
              ;; Escape always leaves the composer.  An empty transcript has
              ;; no response-navigation block yet, but it is still the
              ;; correct focus target.
              (select-window composer-window)
              (with-current-buffer composer
                (e-chat-composer-enter-navigation))
              (with-current-buffer buffer
                (should-not e-chat-response-navigation-mode))
              (select-window composer-window)
              (with-current-buffer composer
                (goto-char (point-max))
                (insert "surface prompt")
                (e-chat-submit))
              (e-board-e2e-drain-session harness session-id)
              (should (equal (plist-get
                              (e-board-e2e-wait-batch harness session-id 1.0)
                              :status)
                             'done))
              (should
               (e-board-e2e-wait-until
                (lambda ()
                  (with-current-buffer buffer
                    (string-match-p "surface answer" (buffer-string))))
                1.0))
              (with-current-buffer buffer
                (should (string-match-p "surface prompt" (buffer-string)))
                (should (string-match-p "surface answer" (buffer-string)))
                (should-not (string-match-p (regexp-quote e-chat--composer-glyph)
                                            (buffer-string))))
              (with-current-buffer composer
                (should (equal (e-chat--composer-text) "")))
              (select-window composer-window)
              (with-current-buffer composer
                (e-chat-composer-enter-navigation))
              (with-current-buffer buffer
                (let ((last-command-event ?x))
                  (should-error (self-insert-command 1)
                                :type 'buffer-read-only))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (set-window-configuration window-configuration))))

(ert-deftest e-chat-surface-e2e-test-new-chat-is-board-native ()
  "The real fresh-chat command creates only board-native persistent state."
  (let* ((e-board--registry (make-hash-table :test 'equal))
         (e-board-registry--boards (make-hash-table :test 'equal))
         (e-board-registry--unsettled-pickup-count 0)
         (e-board-registry--unsettled-effect-count 0)
         (e-board-registry--unsettled-routing-count 0)
         (e-board-registry--unsettled-generation 0)
         (e-board-runtime--attachments (make-hash-table :test 'equal))
         (e-board-runtime--session-attachments (make-hash-table :test 'equal))
         (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
         (e-board-runtime--admission-open-p t)
         (e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
         (e-chat-service--board-bindings (make-hash-table :test 'equal))
         (e-chat-service--board-log-owners (make-hash-table :test 'equal))
         (e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal))
         (e-chat-default-harness-id :chat-e2e)
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         buffer)
    (e-board-e2e-reset-runtime)
    (e-harness-activate-capability
     harness (e-chat-session-capability-create))
    (e-harness-registry-register :chat-e2e harness)
    (unwind-protect
        (progn
          (setq buffer (e-chat-new))
          (should (buffer-live-p buffer))
          (with-current-buffer buffer
            (let* ((board (e-board-registry-get e-chat-board-id))
                   (session (e-session-get (e-harness-sessions harness)
                                           e-chat-session-id))
                   (state (plist-get session :board-session-state)))
              (should (equal e-chat-harness harness))
              (should (eq (e-board-registry-board-state board) 'active))
              (should (equal state
                             (list :board-id e-chat-board-id
                                   :principal
                                   (e-board-registry-board-principal board)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-chat-surface-e2e-test)

;;; e-chat-surface-e2e-test.el ends here

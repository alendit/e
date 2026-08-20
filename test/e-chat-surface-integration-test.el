;;; e-chat-surface-integration-test.el --- Chat surface integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Deterministic batch integration coverage for the interactive chat surface.
;; It uses the fake backend and real Emacs buffer/window objects, but it does
;; not claim graphical redisplay or user-visible viewport behavior.

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
       "../e2e/e-board-e2e-support.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil nil t)

(declare-function e-board-e2e-reset-runtime "e-board-e2e-support")
(declare-function e-board-e2e-drain-session "e-board-e2e-support")
(declare-function e-board-e2e-wait-batch "e-board-e2e-support")
(declare-function e-board-e2e-wait-until "e-board-e2e-support")
(declare-function evil-mode "evil-core")
(declare-function evil-insert-state "evil-states")
(declare-function evil-local-mode "evil-core")
(defvar evil-state)

(defun e-chat-surface-integration--load-evil ()
  "Load Evil only when it is available in the isolated test environment."
  (or (featurep 'evil) (require 'evil nil t)))

(ert-deftest e-chat-surface-integration-test-composer-submits-below-transcript ()
  "A displayed chat keeps input in its pane and responses in its transcript."
  (e-board-e2e-reset-runtime)
  (let* ((backend (e-backend-fake-create
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
                   (composer-window
                    (e-chat--surface-composer-window transcript-window)))
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
              (should (eq (selected-window) transcript-window))
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
              (should (eq (selected-window) transcript-window))
              (with-current-buffer buffer
                (let ((last-command-event ?x))
                  (should-error (self-insert-command 1)
                                :type 'buffer-read-only))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (set-window-configuration window-configuration))))

(ert-deftest e-chat-surface-integration-test-evil-escape-routes-transcript-commands ()
  "One real Evil Escape moves input focus to transcript navigation commands."
  (skip-unless (e-chat-surface-integration--load-evil))
  (evil-mode 1)
  (e-chat--configure-modal-editing-policy)
  (e-chat--configure-evil-composer-bindings)
  (e-board-e2e-reset-runtime)
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "evil answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (session-id "chat-surface-evil-e2e")
         (buffer (e-chat-open :harness harness :session-id session-id))
         (window-configuration (current-window-configuration))
         details-buffer)
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (e-chat--after-display-buffer buffer)
          (with-current-buffer buffer
            (let* ((composer e-chat--surface-composer-buffer)
                   (transcript-window (get-buffer-window buffer t))
                   (composer-window
                    (e-chat--surface-composer-window transcript-window)))
              (select-window composer-window)
              (with-current-buffer composer
                (evil-local-mode 1)
                (evil-insert-state)
                (goto-char (point-max))
                (insert "evil prompt")
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
                    (string-match-p "evil answer" (buffer-string))))
                1.0))
              (select-window composer-window)
              (with-current-buffer composer
                (evil-insert-state)
                (should (eq evil-state 'insert))
                (should (eq (key-binding (kbd "<escape>"))
                            #'e-chat-composer-enter-navigation)))
              (execute-kbd-macro (kbd "<escape>"))
              (should (eq (selected-window) transcript-window))
              (should (eq (current-buffer) buffer))
              (should e-chat-response-navigation-mode)
              (should (eq (key-binding (kbd "RET"))
                          #'e-chat-response-navigation-activate))
              (should (eq (key-binding (kbd "d"))
                          #'e-chat-response-navigation-details))
              ;; Batch Emacs proves the active public command bindings; the
              ;; graphical tier owns command-loop dispatch and redisplay.
              (setq details-buffer
                    (call-interactively (key-binding (kbd "d"))))
              (should (buffer-live-p details-buffer))
              (with-current-buffer details-buffer
                (should (string-match-p "Turn:" (buffer-string)))))))
      (when (buffer-live-p details-buffer)
        (kill-buffer details-buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (set-window-configuration window-configuration))))

(ert-deftest e-chat-surface-integration-test-new-chat-is-board-native ()
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

(provide 'e-chat-surface-integration-test)

;;; e-chat-surface-integration-test.el ends here

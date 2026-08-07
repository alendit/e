;;; e-session-restart-integration-test.el --- Restart integration tests for board sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Deterministic integration coverage for creating a board-backed session,
;; rebuilding the process-local runtime, and resuming the same persisted board.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-session)
(load (expand-file-name
       "../e2e/e-board-e2e-support.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil nil t)

(ert-deftest e-session-restart-integration-test-resumes-persisted-board-session ()
  "A newly created board session keeps its board identity across restart."
  (let* ((directory (make-temp-file "e-session-board-restart-" t))
         (root (file-name-as-directory directory))
         (session-id "board-restart")
         board-id)
    (unwind-protect
        (progn
          (let* ((store (e-session-persistent-index-store-create directory))
                 (backend (e-backend-fake-create
                           :items '((:type assistant-message
                                     :content "before restart")
                                    (:type done :reason stop))))
                 (harness (e-harness-create :backend backend :sessions store)))
            (setq session-id
                  (e-board-e2e-create-session
                   harness :id session-id :metadata (list :project-root root)))
            (e-board-e2e-prompt-batch harness session-id "before restart")
            (setq board-id
                  (plist-get
                   (plist-get (e-session-get store session-id)
                              :board-session-state)
                   :board-id)))
          ;; Model a fresh Emacs process: new store, harness, board registry,
          ;; bindings, and backend, with only the persisted session directory.
          (let* ((store (e-session-persistent-index-store-create directory))
                 (backend (e-backend-fake-create
                           :items '((:type assistant-message
                                     :content "after restart")
                                    (:type done :reason stop))))
                 (harness (e-harness-create :backend backend :sessions store)))
            (e-board-e2e-reset-runtime)
            (e-board-e2e-prompt-batch
             harness session-id "does the board survive restart?")
            (let* ((binding (e-chat-service-binding harness session-id))
                   (board (e-chat-service-binding-board binding))
                   (state (plist-get (e-session-get store session-id)
                                     :board-session-state))
                   (messages (e-harness-messages harness session-id)))
              (should (equal (e-board-registry-board-id board) board-id))
              (should (equal state
                             (list :board-id board-id
                                   :principal (format "chat:%s" session-id))))
              (should (equal (mapcar (lambda (message)
                                       (plist-get message :role))
                                     messages)
                             '(user assistant user assistant)))
              (should (equal (plist-get (car (last messages)) :content)
                             "after restart")))))
      (delete-directory directory t))))

(provide 'e-session-restart-integration-test)

;;; e-session-restart-integration-test.el ends here

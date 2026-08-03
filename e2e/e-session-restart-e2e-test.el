;;; e-session-restart-e2e-test.el --- Restart e2e tests for persisted sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Deterministic end-to-end tests for restarting over existing persisted
;; session state and then using that session through the harness.

;;; Code:

(require 'ert)
(require 'json)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-session)
(load (expand-file-name
       "e-board-e2e-support.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil nil t)

(ert-deftest e-session-restart-e2e-test-legacy-row-allows-fresh-board-chat ()
  "A legacy metadata row cannot prevent a fresh board-backed chat."
  (let* ((directory (make-temp-file "e-session-restart-e2e-" t))
         (root (file-name-as-directory directory))
         (sessions-directory (expand-file-name "sessions" directory))
         (session-id "legacy-restart")
         (session-file (expand-file-name (concat session-id ".jsonl")
                                         sessions-directory)))
    (unwind-protect
        (progn
          (make-directory sessions-directory t)
          (with-temp-file (expand-file-name "index.json" directory)
            (insert
             (json-encode
              (vector
               `(:id ,session-id
                 :created-at "2026-06-29T00:00:00Z"
                 :updated-at "2026-06-29T00:00:00Z"
                 :message-count 0
                 :loaded t
                 :metadata [,root
                            "project-root"
                            "chat-default"
                            "harness-instance-id"])))
             "\n"))
          (with-temp-file session-file
            (insert
             (json-encode
              `(:type "session"
                :session-id ,session-id
                :id "root"
                :timestamp "2026-06-29T00:00:00Z"
                :created-at "2026-06-29T00:00:00Z"
                :updated-at "2026-06-29T00:00:00Z"
                :metadata [,root
                           "project-root"
                           "chat-default"
                           "harness-instance-id"]))
             "\n"))
          (let* ((store (e-session-persistent-index-store-create directory))
                 (backend (e-backend-fake-create
                           :items '((:type assistant-message
                                     :content "after restart")
                                    (:type done :reason stop))))
                 (harness (e-harness-create
                           :backend backend
                           :sessions store)))
            (e-board-e2e-reset-runtime)
            (should (equal (mapcar (lambda (session)
                                     (plist-get session :id))
                                   (e-harness-session-list harness))
                           (list session-id)))
            (let ((metadata (plist-get (e-session-get store session-id)
                                       :metadata)))
              (should (equal (plist-get metadata :project-root) root))
              (should (equal (plist-get metadata :harness-instance-id)
                             "chat-default")))
            (setq session-id
                  (e-board-e2e-create-session
                   harness :id "fresh-after-legacy"
                   :metadata (list :project-root root)))
            (e-board-e2e-prompt-batch
             harness session-id "does chat still work after restart?")
            (let ((metadata (plist-get (e-session-get store session-id)
                                       :metadata))
                  (messages (e-harness-messages harness session-id)))
              (should (equal (plist-get metadata :project-root) root))
              (should (equal (mapcar (lambda (message)
                                       (plist-get message :role))
                                     messages)
                             '(user assistant)))
              (should (equal (plist-get (cadr messages) :content)
                             "after restart")))))
      (delete-directory directory t))))

(provide 'e-session-restart-e2e-test)

;;; e-session-restart-e2e-test.el ends here

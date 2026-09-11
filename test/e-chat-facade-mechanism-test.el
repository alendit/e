;;; e-chat-facade-mechanism-test.el --- Chat facade mechanism tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Direct profiling assertions for the chat facade.  Multi-owner presentation
;; behavior remains in `e-chat-presentation-integration-test.el'.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-chat)
(require 'e-dev-profile)
(require 'e-harness)
(require 'e-session)
(require 'e-ui-work)
(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-chat-test-profile-records-pending-activity-redraw ()
  "Enabled dev profiling records the actual deferred activity redraw body."
  (let ((buffer (e-chat-test--buffer nil "chat-profile-activity-redraw"))
        (profile-directory (make-temp-file "e-chat-profile-" t))
        (e-dev-profile-directory nil)
        (e-dev-profile--enabled nil)
        (e-dev-profile--current-file nil)
        (e-dev-profile--latest-file nil))
    (setq e-dev-profile-directory profile-directory)
    (unwind-protect
        (with-current-buffer buffer
          (e-chat-render-event
           (e-events-make :type 'turn-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :created-at 10))
          (e-chat-render-event
           (e-events-make :type 'tool-started
                          :session-id e-chat-session-id
                          :turn-id "turn-1"
                          :payload '(:type tool-call
                                      :id "call-1"
                                      :name "read")))
          (e-dev-profile-start)
          (e-chat-activity-run-pending-redraw)
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates)))
            (should (alist-get "chat.activity-redraw" aggregates nil nil #'equal))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory profile-directory t))))

(ert-deftest e-chat-test-profile-records-submit-action ()
  "Enabled dev profiling records the chat submit command."
  (let* ((profile-directory (make-temp-file "e-chat-profile-" t))
         (e-dev-profile-directory profile-directory)
         (e-dev-profile--enabled nil)
         (e-dev-profile--current-file nil)
         (e-dev-profile--latest-file nil)
         (backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (buffer (e-chat-open :harness harness
                              :session-id "chat-submit-profile")))
    (unwind-protect
        (with-current-buffer (e-chat-test--composer buffer)
          (goto-char (point-max))
          (insert "profile submit")
          (e-dev-profile-start)
          (e-chat-submit)
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates))
                 (records
                  (e-dev-profile--read-json-lines e-dev-profile--latest-file))
                 (submit-record
                  (seq-find (lambda (record)
                              (equal (alist-get 'event record) "chat.submit"))
                            records))
                 (metadata (alist-get 'metadata submit-record)))
            (should (alist-get "chat.submit" aggregates nil nil #'equal))
            (should submit-record)
            (should (equal (alist-get 'session-id submit-record)
                           "chat-submit-profile"))
            (should (equal (alist-get 'intent metadata) "submit"))
            (should (= (alist-get 'prompt-chars metadata) 14))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory profile-directory t))))

(provide 'e-chat-facade-mechanism-test)

;;; e-chat-facade-mechanism-test.el ends here

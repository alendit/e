;;; e-ui-work-integration-test.el --- UI work integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Deterministic integration tests for async presentation work.  These tests
;; avoid live providers and assert lifecycle shape rather than elapsed-time
;; thresholds.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-chat)
(require 'e-harness)
(require 'e-ui-work)
(load (expand-file-name
       "../e2e/e-chat-sql-e2e-support.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil nil t)

(defun e-ui-work-integration--drain (buffer &rest args)
  "Drain finite UI work in BUFFER with ARGS."
  (e-ui-work-with-batch-drain
    (apply #'e-ui-work-drain-batch :buffer buffer :timeout 5.0 args)))

(ert-deftest e-ui-work-integration-test-chat-settled-turn-leaves-no-pending-ui-work ()
  "A settled chat turn cancels intervals and drains finite UI work."
  (let* ((harness (e-chat-sql-e2e-make-harness
                   :backend (e-backend-fake-create :items nil)))
         (session-id (e-chat-sql-e2e-create-session
                      harness :id "ui-work-e2e"))
         (turn-id "ui-work-turn")
         (buffer (e-chat-open :harness harness
                              :session-id session-id
                              :new-session nil))
         (window-configuration (current-window-configuration)))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (let ((e-chat-activity-redraw-delay 0)
                (e-chat-progress-interval 0.02)
                (e-chat-deferred-markdown-threshold-bytes 8)
                (e-chat-deferred-markdown-chunk-lines 1)
                (markdown (string-join
                           (cl-loop for index below 24
                                    collect (format "- **item %02d** [ref](https://example.test/%02d)"
                                                    index
                                                    index))
                           "\n")))
            (e-chat-render-event
             (list :type 'turn-started
                   :session-id session-id
                   :turn-id turn-id
                   :created-at (float-time)
                   :payload nil))
            (should (e-ui-work-pending buffer
                                       :owner 'progress-indicator
                                       :key turn-id))
            (dotimes (index 8)
              (e-chat-render-event
               (list :type 'reasoning-delta
                     :session-id session-id
                     :turn-id turn-id
                     :created-at (float-time)
                     :payload (list :type 'reasoning-delta
                                    :content (format "thought %d" index)))))
            (should (e-ui-work-pending buffer
                                       :owner 'activity-redraw
                                       :key turn-id))
            (e-ui-work-integration--drain buffer
                                           :owner 'activity-redraw
                                           :key turn-id)
            (should-not (e-ui-work-pending buffer
                                           :owner 'activity-redraw
                                           :key turn-id))
            (should (e-ui-work-pending buffer
                                       :owner 'progress-indicator
                                       :key turn-id))
            (e-chat-render-event
             (list :type 'message-added
                   :session-id session-id
                   :turn-id turn-id
                   :created-at (float-time)
                   :payload (list :message
                                  (list :role 'assistant
                                        :turn-id turn-id
                                        :content markdown))))
            ;; Durable assistant output precedes terminal lifecycle on the
            ;; board.  Output alone must not settle progress; otherwise the UI
            ;; flickers between a final summary and live progress.
            (should (e-ui-work-pending buffer
                                       :owner 'progress-indicator
                                       :key turn-id))
            (should (e-ui-work-pending buffer :owner 'markdown-presentation))
            (e-ui-work-integration--drain buffer :owner 'markdown-presentation)
            (should-not (e-ui-work-pending buffer :owner 'markdown-presentation))
            (e-chat-render-event
             (list :type 'turn-finished
                   :session-id session-id
                   :turn-id turn-id
                   :created-at (float-time)
                   :payload nil))
            (should-not (e-ui-work-pending buffer
                                           :owner 'progress-indicator
                                           :key turn-id))
            (e-ui-work-integration--drain buffer)
            (should-not (e-ui-work-pending buffer)))))
      (set-window-configuration window-configuration)
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (e-chat-sql-e2e-reset))))

(provide 'e-ui-work-integration-test)

;;; e-ui-work-integration-test.el ends here

;;; e-ui-work-e2e-test.el --- UI work e2e tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Deterministic end-to-end tests for async presentation work.  These tests
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

(defun e-ui-work-e2e--drain (buffer &rest args)
  "Drain finite UI work in BUFFER with ARGS."
  (e-ui-work-with-batch-drain
    (apply #'e-ui-work-drain-batch :buffer buffer args)))

(ert-deftest e-ui-work-e2e-test-chat-settled-turn-leaves-no-pending-ui-work ()
  "A settled chat turn cancels intervals and drains finite UI work."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (session-id "ui-work-e2e")
         (turn-id "ui-work-turn")
         (buffer (e-chat-open :harness harness
                              :session-id session-id
                              :new-session nil)))
    (unwind-protect
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
            (e-chat--render-event
             (list :type 'turn-started
                   :session-id session-id
                   :turn-id turn-id
                   :created-at (float-time)
                   :payload nil))
            (should (e-ui-work-pending buffer
                                       :owner 'progress-indicator
                                       :key turn-id))
            (dotimes (index 8)
              (e-chat--render-event
               (list :type 'reasoning-delta
                     :session-id session-id
                     :turn-id turn-id
                     :created-at (float-time)
                     :payload (list :type 'reasoning-delta
                                    :content (format "thought %d" index)))))
            (should (e-ui-work-pending buffer
                                       :owner 'activity-redraw
                                       :key turn-id))
            (e-ui-work-e2e--drain buffer
                                   :owner 'activity-redraw
                                   :key turn-id)
            (should-not (e-ui-work-pending buffer
                                           :owner 'activity-redraw
                                           :key turn-id))
            (should (e-ui-work-pending buffer
                                       :owner 'progress-indicator
                                       :key turn-id))
            (e-chat--render-event
             (list :type 'message-added
                   :session-id session-id
                   :turn-id turn-id
                   :created-at (float-time)
                   :payload (list :message
                                  (list :role 'assistant
                                        :turn-id turn-id
                                        :content markdown))))
            (should-not (e-ui-work-pending buffer
                                           :owner 'progress-indicator
                                           :key turn-id))
            (should (e-ui-work-pending buffer :owner 'markdown-presentation))
            (e-ui-work-e2e--drain buffer :owner 'markdown-presentation)
            (should-not (e-ui-work-pending buffer :owner 'markdown-presentation))
            (e-chat--render-event
             (list :type 'turn-finished
                   :session-id session-id
                   :turn-id turn-id
                   :created-at (float-time)
                   :payload nil))
            (e-ui-work-e2e--drain buffer)
            (should-not (e-ui-work-pending buffer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'e-ui-work-e2e-test)

;;; e-ui-work-e2e-test.el ends here

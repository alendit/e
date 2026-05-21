;;; e-loop.el --- Agent turn loop for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider-neutral synchronous turn loop for core runtime tests.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-backend)
(require 'e-tools)

(define-error 'e-loop-backend-error "Backend returned an error")
(define-error 'e-loop-empty-output "Backend returned no assistant output")

(defvar e-loop--message-counter 0
  "Monotonic message id counter for loop-created messages.")

(defun e-loop--next-message-id ()
  "Return a new in-process message id."
  (setq e-loop--message-counter (1+ e-loop--message-counter))
  (format "msg-%d" e-loop--message-counter))

(defun e-loop--assistant-message (content &optional metadata)
  "Return an assistant message with CONTENT and optional METADATA."
  (list :id (e-loop--next-message-id)
        :role 'assistant
        :content content
        :metadata metadata))

(cl-defun e-loop--emit (&key on-event type payload)
  "Report internal turn descriptor TYPE and PAYLOAD through ON-EVENT."
  (funcall on-event type payload))

(cl-defun e-loop-run-turn
    (&key session-id turn-id messages backend tools options on-event append-message)
  "Run one agent turn for SESSION-ID and TURN-ID.
MESSAGES, BACKEND, TOOLS, OPTIONS, ON-EVENT, and APPEND-MESSAGE define the
turn context and output callbacks.
The loop is synchronous for the first core implementation.  Async process
management stays outside this task until the core event and state semantics are
stable."
  (ignore session-id turn-id)
  (let ((assistant-content nil)
        (assistant-message-written nil)
        (done-reason nil)
        (turn-messages (copy-sequence messages))
        (continue t))
    (e-loop--emit :on-event on-event
                  :type 'turn-started
                  :payload nil)
    (while continue
      (let ((tool-called nil)
            (response-assistant-content nil)
            (response-assistant-message nil))
        (e-backend-stream
         backend
         :messages turn-messages
         :options options
         :on-item
         (lambda (item)
           (pcase (plist-get item :type)
             ('assistant-delta
              (setq response-assistant-content
                    (concat response-assistant-content
                            (plist-get item :content)))
              (e-loop--emit :on-event on-event
                            :type 'assistant-delta
                            :payload item))
             ('assistant-message
              (setq response-assistant-message
                    (plist-get item :content)))
             ('reasoning-delta
              (e-loop--emit :on-event on-event
                            :type 'reasoning-delta
                            :payload item))
             ('tool-call
              (setq tool-called t)
              (let* ((tool-call-message
                      (list :id (e-loop--next-message-id)
                            :role 'tool-call
                            :content item
                            :metadata nil)))
                (e-loop--emit :on-event on-event
                              :type 'tool-started
                              :payload item)
                (let* ((result (e-tools-execute tools item))
                       (message (list :id (e-loop--next-message-id)
                                      :role 'tool
                                      :content result
                                      :metadata nil)))
                  (setq turn-messages
                        (append turn-messages (list tool-call-message)))
                  (funcall append-message tool-call-message)
                  (setq turn-messages (append turn-messages (list message)))
                  (funcall append-message message)
                  (e-loop--emit :on-event on-event
                                :type 'tool-finished
                                :payload (list :tool-call item
                                               :result result)))))
             ('done
              (setq done-reason (plist-get item :reason)))
             ('backend-error
              (signal 'e-loop-backend-error
                      (list (plist-get item :content))))
             (_
              (e-loop--emit :on-event on-event
                            :type 'backend-item-ignored
                            :payload item)))))
        (let ((response-text
               (or response-assistant-message
                   response-assistant-content)))
          (if tool-called
              (progn
                (when (not (string-empty-p (or response-text "")))
                  (e-loop--emit :on-event on-event
                                :type 'reasoning-delta
                                :payload (list :type 'reasoning-delta
                                               :content response-text)))
                (setq continue t))
            (setq continue nil)
            (when (not (string-empty-p (or response-text "")))
              (setq assistant-content response-text)
              (setq assistant-message-written t)
              (let ((message (e-loop--assistant-message response-text)))
                (setq turn-messages (append turn-messages (list message)))
                (funcall append-message message)))))))
    (cond
     ((and (not assistant-message-written)
           (string-empty-p (or assistant-content "")))
      (e-loop--emit :on-event on-event
                    :type 'backend-empty-output
                    :payload (list :reason done-reason))
      (signal 'e-loop-empty-output nil)))
    (e-loop--emit :on-event on-event
                  :type 'turn-finished
                  :payload (list :reason done-reason))
    (list :status 'done
          :reason done-reason
          :assistant-content assistant-content)))

(provide 'e-loop)

;;; e-loop.el ends here

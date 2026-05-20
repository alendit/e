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
(require 'e-events)
(require 'e-tools)

(define-error 'e-loop-backend-error "Backend returned an error")

(defvar e-loop--message-counter 0
  "Monotonic message id counter for loop-created messages.")

(defun e-loop--next-message-id ()
  "Return a new in-process message id."
  (setq e-loop--message-counter (1+ e-loop--message-counter))
  (format "msg-%d" e-loop--message-counter))

(defun e-loop--assistant-message (content)
  "Return an assistant message with CONTENT."
  (list :id (e-loop--next-message-id)
        :role 'assistant
        :content content
        :metadata nil))

(cl-defun e-loop--emit (&key on-event session-id turn-id type payload)
  "Emit event TYPE for SESSION-ID and TURN-ID through ON-EVENT.
PAYLOAD is stored as the event payload."
  (funcall on-event
           (e-events-make :type type
                          :session-id session-id
                          :turn-id turn-id
                          :payload payload)))

(cl-defun e-loop-run-turn
    (&key session-id turn-id messages backend tools options on-event append-message)
  "Run one agent turn for SESSION-ID and TURN-ID.
MESSAGES, BACKEND, TOOLS, OPTIONS, ON-EVENT, and APPEND-MESSAGE define the
turn context and output callbacks.
The loop is synchronous for the first core implementation.  Async process
management stays outside this task until the core event and state semantics are
stable."
  (let ((assistant-content nil)
        (assistant-message-written nil)
        (done-reason nil)
        (turn-messages (copy-sequence messages))
        (continue t)
        (iteration 0)
        (max-iterations (or (plist-get options :max-tool-iterations) 4)))
    (e-loop--emit :on-event on-event
                  :session-id session-id
                  :turn-id turn-id
                  :type 'turn-started
                  :payload nil)
    (while continue
      (setq iteration (1+ iteration))
      (let ((tool-called nil)
            (assistant-message nil))
        (e-backend-stream
         backend
         :messages turn-messages
         :options options
         :on-item
         (lambda (item)
           (pcase (plist-get item :type)
             ('assistant-delta
              (setq assistant-content
                    (concat assistant-content (plist-get item :content)))
              (e-loop--emit :on-event on-event
                            :session-id session-id
                            :turn-id turn-id
                            :type 'assistant-delta
                            :payload item))
             ('assistant-message
              (setq assistant-message t)
              (setq assistant-message-written t)
              (let ((message (e-loop--assistant-message
                              (plist-get item :content))))
                (setq turn-messages (append turn-messages (list message)))
                (funcall append-message message)
                (e-loop--emit :on-event on-event
                              :session-id session-id
                              :turn-id turn-id
                              :type 'message-added
                              :payload (list :message message))))
             ('tool-call
              (setq tool-called t)
              (let* ((tool-call-message
                      (list :id (e-loop--next-message-id)
                            :role 'tool-call
                            :content item
                            :metadata nil))
                     (result (e-tools-execute tools item))
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
                              :session-id session-id
                              :turn-id turn-id
                              :type 'tool-finished
                              :payload (list :result result))))
             ('done
              (setq done-reason (plist-get item :reason)))
             ('backend-error
              (signal 'e-loop-backend-error
                      (list (plist-get item :content))))
             (_
              (e-loop--emit :on-event on-event
                            :session-id session-id
                            :turn-id turn-id
                            :type 'backend-item-ignored
                            :payload item)))))
        (setq continue (and tool-called
                            (not assistant-message)
                            (< iteration max-iterations)))))
    (cond
     ((and (not assistant-message-written)
           (not (string-empty-p (or assistant-content ""))))
      (let ((message (e-loop--assistant-message assistant-content)))
        (funcall append-message message)
        (e-loop--emit :on-event on-event
                      :session-id session-id
                      :turn-id turn-id
                      :type 'message-added
                      :payload (list :message message))))
     ((and (not assistant-message-written)
           (string-empty-p (or assistant-content "")))
      (e-loop--emit :on-event on-event
                    :session-id session-id
                    :turn-id turn-id
                    :type 'backend-empty-output
                    :payload (list :reason done-reason))))
    (e-loop--emit :on-event on-event
                  :session-id session-id
                  :turn-id turn-id
                  :type 'turn-finished
                  :payload (list :reason done-reason))
    (list :status 'done
          :reason done-reason
          :assistant-content assistant-content)))

(provide 'e-loop)

;;; e-loop.el ends here

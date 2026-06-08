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

(defun e-loop--assistant-message (content &optional metadata)
  "Return an assistant message with CONTENT and optional METADATA."
  (list :role 'assistant
        :content content
        :metadata metadata))

(cl-defun e-loop--emit (&key on-event type payload)
  "Report internal turn descriptor TYPE and PAYLOAD through ON-EVENT."
  (funcall on-event type payload))

(defun e-loop--request-lifecycle-payload (request status &optional started-at)
  "Return sanitized lifecycle payload for REQUEST with STATUS.
STARTED-AT is the `float-time' value captured when the provider request was
published, used to calculate `:elapsed-seconds' for finished events."
  (let* ((metadata (and (e-backend-request-p request)
                        (e-backend-request-metadata request)))
         (payload (list :provider (plist-get metadata :provider)
                        :transport (plist-get metadata :transport)
                        :url-host (plist-get metadata :url-host)
                        :url-path (plist-get metadata :url-path)
                        :timeout-seconds (plist-get metadata
                                                     :timeout-seconds)
                        :status status)))
    (when started-at
      (plist-put payload
                 :elapsed-seconds
                 (/ (float (round (* 1000 (max 0.0
                                                (- (float-time)
                                                   started-at)))))
                    1000)))
    payload))

(cl-defun e-loop-start-turn
    (&key session-id turn-id messages backend tools tool-lifecycle options on-event
          append-message refresh-messages on-request-start on-done on-error
          cancelled-p)
  "Start one async agent turn for SESSION-ID and TURN-ID.
MESSAGES, BACKEND, TOOLS, TOOL-LIFECYCLE, and OPTIONS describe the turn input.  ON-EVENT,
APPEND-MESSAGE, REFRESH-MESSAGES, ON-REQUEST-START, ON-DONE, ON-ERROR, and
CANCELLED-P receive turn progress, output, refreshed context, provider request
handles, settlement, failures, and cancellation state.  The provider request is
started through `e-backend-start'.  Tool execution is started through
TOOL-LIFECYCLE when supplied, otherwise through `e-tools-start'.  Provider I/O,
tool I/O, and turn settlement are callback-driven."
  (ignore session-id turn-id)
  (let ((turn-messages (copy-sequence messages))
        (settled nil)
        (active-request nil))
    (cl-labels
        ((cancelled ()
           (and cancelled-p (funcall cancelled-p)))
         (fail
          (err)
          (unless settled
            (setq settled t)
            (when on-error
              (funcall on-error err))))
         (finish
          (done-reason assistant-content)
          (unless settled
            (setq settled t)
            (e-loop--emit :on-event on-event
                          :type 'turn-finished
                          :payload (list :reason done-reason))
            (when on-done
              (funcall on-done
                       (list :status 'done
                             :reason done-reason
                             :assistant-content assistant-content)))))
         (publish-request
          (request)
          (setq active-request request)
          (when on-request-start
            (funcall on-request-start request)))
         (start-request
          ()
          (unless (or settled (cancelled))
            (let ((tool-called nil)
                  (tool-queue nil)
                  (active-tool nil)
                  (provider-done nil)
                  (followup-started nil)
                  (response-assistant-content nil)
                  (response-assistant-message nil)
                  (token-usage nil)
                  (done-reason nil)
                  (provider-request nil)
                  (provider-request-started-at nil)
                  (provider-request-finished nil))
              (cl-labels
                  ((response-text ()
                     (or response-assistant-message
                         response-assistant-content))
                   (publish-provider-request
                    (request)
                    (setq provider-request request)
                    (setq provider-request-started-at (float-time))
                    (setq provider-request-finished nil)
                    (publish-request request)
                    (e-loop--emit
                     :on-event on-event
                     :type 'provider-request-started
                     :payload
                     (e-loop--request-lifecycle-payload
                      request 'started)))
                   (finish-provider-request
                    (status)
                    (when (and provider-request
                               (not provider-request-finished))
                      (setq provider-request-finished t)
                      (e-loop--emit
                       :on-event on-event
                       :type 'provider-request-finished
                       :payload
                       (e-loop--request-lifecycle-payload
                        provider-request status
                        provider-request-started-at))))
                   (fail-provider
                    (err)
                    (finish-provider-request 'error)
                    (fail err))
                   (maybe-start-followup
                    ()
                    (when (and provider-done
                               tool-called
                               (not active-tool)
                               (null tool-queue)
                               (not followup-started)
                               (not settled)
                               (not (cancelled)))
                      (setq followup-started t)
                      (when (not (string-empty-p
                                  (or (response-text) "")))
                        (e-loop--emit
                         :on-event on-event
                         :type 'reasoning-delta
                         :payload
                         (list :type 'reasoning-delta
                               :content (response-text))))
                      (start-request)))
                   (finish-tool
                    (tool-call result)
                    (unless (or settled (cancelled))
                      (setq active-tool nil)
                      (let ((message
                             (list :role 'tool
                                   :content result
                                   :metadata (plist-get result :metadata))))
                        (setq turn-messages
                              (append turn-messages (list message)))
                        (funcall append-message message)
                        (e-loop--emit
                         :on-event on-event
                         :type 'tool-finished
                         :payload (list :tool-call tool-call
                                        :result result)))
                      (when (and refresh-messages
                                 (plist-get (plist-get result :metadata)
                                            :refresh-context))
                        (setq turn-messages (funcall refresh-messages)))
                      (start-next-tool)
                      (maybe-start-followup)))
                   (start-next-tool
                    ()
                    (when (and (not active-tool)
                               tool-queue
                               (not settled)
                               (not (cancelled)))
                      (condition-case err
                          (let* ((entry (pop tool-queue))
                                 (tool-call
                                  (if tool-lifecycle
                                      (e-tool-lifecycle-prepare-call
                                       tool-lifecycle
                                       (plist-get entry :tool-call))
                                    (plist-get entry :tool-call)))
                                 (tool-call-message
                                  (list :role 'tool-call
                                        :content tool-call
                                        :metadata nil)))
                            (setq active-tool t)
                            (setq turn-messages
                                  (append turn-messages
                                          (list tool-call-message)))
                            (funcall append-message tool-call-message)
                            (e-loop--emit :on-event on-event
                                          :type 'tool-started
                                          :payload tool-call)
                            (let ((request
                                   (if tool-lifecycle
                                       (e-tool-lifecycle-start-call
                                        tool-lifecycle
                                        tool-call
                                        :on-request-start
                                        (lambda (request)
                                          (setq active-tool request)
                                          (publish-request request))
                                        :on-done
                                        (lambda (result)
                                          (finish-tool tool-call result))
                                        :on-error #'fail)
                                     (e-tools-start
                                      tools
                                      tool-call
                                      :on-request-start
                                      (lambda (request)
                                        (setq active-tool request)
                                        (publish-request request))
                                      :on-done
                                      (lambda (result)
                                        (finish-tool tool-call result))
                                      :on-error #'fail))))
                              (when request
                                (setq active-tool request))))
                        (error
                         (fail err)))))
                  (enqueue-tool-call
                   (item)
                    (setq tool-called t)
                    (setq tool-queue
                          (append tool-queue
                                  (list (list :tool-call item))))
                    (start-next-tool)))
              (setq
               active-request
               (condition-case err
                   (let ((reported-request nil))
                     (let ((request
                            (e-backend-start
                             backend
                             :messages turn-messages
                             :options options
                             :on-request-start
                             (lambda (request)
                               (setq reported-request request)
                               (publish-provider-request request))
                             :on-item
                             (lambda (item)
                               (unless (or settled (cancelled))
                                 (condition-case err
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
                                        (enqueue-tool-call item))
                                       ('token-usage
                                        (setq token-usage
                                              (plist-get item :usage))
                                        (e-loop--emit
                                         :on-event on-event
                                         :type 'token-usage
                                         :payload token-usage))
                                       ('done
                                        (setq done-reason
                                              (plist-get item :reason)))
                                       ('backend-error
                                        (fail-provider
                                         (list 'e-loop-backend-error
                                               (plist-get item :content)
                                               (plist-get item :payload))))
                                       (_
                                        (e-loop--emit
                                         :on-event on-event
                                         :type 'backend-item-ignored
                                         :payload item)))
                                   (error
                                    (fail-provider err)))))
                             :on-done
                             (lambda (_backend-result)
                               (unless (or settled (cancelled))
                                 (condition-case err
                                     (progn
                                       (finish-provider-request 'done)
                                       (setq provider-done t)
                                       (if tool-called
                                           (maybe-start-followup)
                                         (if (string-empty-p
                                              (or (response-text) ""))
                                             (progn
                                               (e-loop--emit
                                                :on-event on-event
                                                :type 'backend-empty-output
                                                :payload (list :reason
                                                               done-reason))
                                               (fail '(e-loop-empty-output)))
                                           (let ((message
                                                  (e-loop--assistant-message
                                                   (response-text))))
                                             (setq turn-messages
                                                   (append turn-messages
                                                           (list message)))
                                             (funcall append-message message)
                                             (finish done-reason
                                                     (response-text))))))
                                   (error
                                    (fail-provider err)))))
                             :on-error #'fail-provider)))
                       (when (and request
                                  (not settled)
                                  (not (eq request reported-request)))
                         (publish-provider-request request))
                       request))
                 (error
                  (fail-provider err)
                  nil))))))))
      (e-loop--emit :on-event on-event
                    :type 'turn-started
                    :payload nil)
      (start-request)
      active-request)))

(cl-defun e-loop-run-turn
    (&key session-id turn-id messages backend tools tool-lifecycle options on-event
          append-message refresh-messages on-request-start)
  "Run one agent turn for SESSION-ID and TURN-ID.
MESSAGES, BACKEND, TOOLS, TOOL-LIFECYCLE, OPTIONS, ON-EVENT, APPEND-MESSAGE,
and REFRESH-MESSAGES define the turn context and output callbacks.
ON-REQUEST-START receives the backend request handle when an adapter exposes
one.  This is a blocking convenience wrapper over `e-loop-start-turn'."
  (let ((done nil)
        (result nil)
        (failure nil))
    (e-loop-start-turn
     :session-id session-id
     :turn-id turn-id
     :messages messages
     :backend backend
     :tools tools
     :tool-lifecycle tool-lifecycle
     :options options
     :on-event on-event
     :append-message append-message
     :refresh-messages refresh-messages
     :on-request-start on-request-start
     :on-done (lambda (value)
                (setq result value)
                (setq done t))
     :on-error (lambda (err)
                 (setq failure err)
                 (setq done t)))
    (while (not done)
      (accept-process-output nil 0.01))
    (when failure
      (signal (car failure) (cdr failure)))
    result))

(provide 'e-loop)

;;; e-loop.el ends here

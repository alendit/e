;;; e-provider-continuation-integration-test.el --- SQL chat/provider integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider lifecycle tests through the public SQL-backed chat service.  Each
;; test owns a disposable SQLite store; no Board aggregate fixture is involved.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'seq)
(require 'e-chat-service)
(require 'e-harness-activity)
(require 'e-openai)
(require 'e-session-sqlite)
(require 'e-work)

(defun e-provider-continuation-integration--await (work)
  "Observe request-scoped WORK at this explicit test boundary."
  (e-work-with-batch-await
    (e-work-await-batch work :timeout 5.0)))

(defun e-provider-continuation-integration--wait-until
    (predicate &optional timeout)
  "Wait up to TIMEOUT seconds for PREDICATE while pumping async callbacks."
  (let ((deadline (+ (float-time) (or timeout 2.0))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (funcall predicate)))

(defun e-provider-continuation-integration--start-session
    (harness session-id prompt)
  "Create SESSION-ID and admit PROMPT through HARNESS's public SQL service."
  (let ((creation
         (e-chat-service-create-session-start
          :harness harness :id session-id
          :metadata (list :name session-id))))
    (e-provider-continuation-integration--await
     (e-chat-service-submit-session harness session-id prompt))
    (e-provider-continuation-integration--await creation)
    (should (e-chat-service-binding harness session-id))))

(defun e-provider-continuation-integration--finish-socket
    (socket callbacks response-id text)
  "Finish SOCKET through CALLBACKS with RESPONSE-ID and assistant TEXT."
  (let ((on-message (gethash socket callbacks)))
    (should on-message)
    (funcall on-message socket
             (json-encode
              (list :type "response.output_text.done" :text text)))
    (funcall on-message socket
             (json-encode
              (list :type "response.completed"
                    :response (list :id response-id :status "completed"))))))

(defun e-provider-continuation-integration--terminal-type-p (events type)
  "Return non-nil when EVENTS contain terminal TYPE."
  (seq-find (lambda (event) (eq (plist-get event :type) type)) events))

(cl-defmacro e-provider-continuation-integration--with-store
    ((store directory harness-form) &rest body)
  "Run BODY with disposable SQLite STORE from DIRECTORY and HARNESS-FORM."
  (declare (indent 1) (debug ((symbolp symbolp form) body)))
  `(let* ((,directory (make-temp-file "e-provider-sql-" t))
          (,store (e-session-sqlite-store-create ,directory))
          (harness ,harness-form))
     (unwind-protect
         (progn ,@body)
       (dolist (session-id '("session-one" "session-two"))
         (when-let* ((binding (e-chat-service-binding harness session-id)))
           (ignore-errors (e-chat-service-abort-session harness session-id))
           (ignore-errors (e-chat-service--retire-binding binding))))
       (ignore-errors (e-session-sqlite-store-close ,store))
       (when (file-directory-p ,directory)
         (delete-directory ,directory t)))))

(defconst e-provider-continuation-integration--provider-profile
  '((continuation-websocket-e2e
     :name "Continuation WebSocket E2E"
     :base-url "https://gateway.example.test/v1"
     :auth bearer
     :env-key "OPENAI_GATEWAY_API_KEY"
     :wire-api responses
     :responses-transport websocket
     :response-store t
     :continuation t
     :requires-openai-auth nil))
  "Provider profile used by isolated SQL-backed websocket tests.")

(ert-deftest e-provider-continuation-integration-test-concurrent-sql-sessions-isolate-sockets ()
  "Concurrent public SQL chat sessions own distinct provider requests."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-openai-websocket-idle-timeout-seconds nil)
         (e-openai-model-providers
          e-provider-continuation-integration--provider-profile)
         (callbacks (make-hash-table :test 'eq))
         (open-count 0)
         sockets)
    (e-provider-continuation-integration--with-store
        (store directory
               (e-openai-create-harness
                :provider 'continuation-websocket-e2e
                :model "gpt-test" :sessions store))
      (cl-letf (((symbol-function 'websocket-open)
                 (lambda (_url &rest arguments)
                   (let ((socket
                          (intern (format "fake-websocket-%d"
                                          (cl-incf open-count)))))
                     (puthash socket (plist-get arguments :on-message) callbacks)
                     (push socket sockets)
                     socket)))
                ((symbol-function 'websocket-send-text)
                 (lambda (_socket _text) t))
                ((symbol-function 'websocket-close) (lambda (&rest _) t)))
        (e-provider-continuation-integration--start-session
         harness "session-one" "first request")
        (e-provider-continuation-integration--start-session
         harness "session-two" "second request")
        (should
         (e-provider-continuation-integration--wait-until
          (lambda () (= open-count 2))))
        (should (= (length (delete-dups (copy-sequence sockets))) 2))
        (e-provider-continuation-integration--finish-socket
         (nth 0 sockets) callbacks "response-one" "first done")
        (e-provider-continuation-integration--finish-socket
         (nth 1 sockets) callbacks "response-two" "second done")
        (should
         (e-provider-continuation-integration--wait-until
          (lambda ()
            (and (not (e-chat-service-active-turn-p harness "session-one"))
                 (not (e-chat-service-active-turn-p harness "session-two"))))))
        (should (e-chat-service-binding harness "session-one"))
        (should (e-chat-service-binding harness "session-two"))))))

(ert-deftest e-provider-continuation-integration-test-cancel-is-request-local ()
  "Cancelling one SQL chat request leaves a concurrent request runnable."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-openai-websocket-idle-timeout-seconds nil)
         (e-openai-model-providers
          e-provider-continuation-integration--provider-profile)
         (callbacks (make-hash-table :test 'eq))
         (socket-by-prompt (make-hash-table :test 'equal))
         (events (make-hash-table :test 'equal))
         (open-count 0)
         sockets)
    (e-provider-continuation-integration--with-store
        (store directory
               (e-openai-create-harness
                :provider 'continuation-websocket-e2e
                :model "gpt-test" :sessions store))
      (dolist (session-id '("session-one" "session-two"))
        (e-harness-activity-subscribe
         harness
         (lambda (event)
           (push (copy-tree event t)
                 (gethash (plist-get event :session-id) events)))
         :session-id session-id))
      (cl-letf (((symbol-function 'websocket-open)
                 (lambda (_url &rest arguments)
                   (let ((socket
                          (intern (format "fake-websocket-%d"
                                          (cl-incf open-count)))))
                     (puthash socket (plist-get arguments :on-message) callbacks)
                     (setq sockets (append sockets (list socket)))
                     socket)))
                ((symbol-function 'websocket-send-text)
                 (lambda (socket text)
                   (dolist (prompt '("cancel me" "keep running"))
                     (when (string-match-p (regexp-quote prompt) text)
                       (puthash prompt socket socket-by-prompt)))
                   t))
                ((symbol-function 'websocket-close) (lambda (&rest _) t)))
        (e-provider-continuation-integration--start-session
         harness "session-one" "cancel me")
        (e-provider-continuation-integration--start-session
         harness "session-two" "keep running")
        (should
         (e-provider-continuation-integration--wait-until
          (lambda () (= open-count 2))))
        (e-chat-service-abort-session harness "session-one")
        (should
         (e-provider-continuation-integration--wait-until
          (lambda ()
            (e-provider-continuation-integration--terminal-type-p
             (gethash "session-one" events) 'turn-cancelled))))
        (should (e-chat-service-active-turn-p harness "session-two"))
        (should-not
         (e-provider-continuation-integration--terminal-type-p
          (gethash "session-two" events) 'turn-cancelled))
        (e-provider-continuation-integration--finish-socket
         (gethash "keep running" socket-by-prompt)
         callbacks "response-two" "survived cancellation")
        (should
         (e-provider-continuation-integration--wait-until
          (lambda ()
            (e-provider-continuation-integration--terminal-type-p
             (gethash "session-two" events) 'turn-finished))))))))

(ert-deftest e-provider-continuation-integration-test-terminal-error-is-request-local ()
  "A terminal provider error fails one SQL session while its sibling completes."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-openai-websocket-idle-timeout-seconds nil)
         (e-openai-model-providers
          e-provider-continuation-integration--provider-profile)
         (callbacks (make-hash-table :test 'eq))
         (socket-by-prompt (make-hash-table :test 'equal))
         (events (make-hash-table :test 'equal))
         (open-count 0)
         sockets)
    (e-provider-continuation-integration--with-store
        (store directory
               (e-openai-create-harness
                :provider 'continuation-websocket-e2e
                :model "gpt-test" :sessions store))
      (dolist (session-id '("session-one" "session-two"))
        (e-harness-activity-subscribe
         harness
         (lambda (event)
           (push (copy-tree event t)
                 (gethash (plist-get event :session-id) events)))
         :session-id session-id))
      (cl-letf (((symbol-function 'websocket-open)
                 (lambda (_url &rest arguments)
                   (let ((socket
                          (intern (format "fake-websocket-%d"
                                          (cl-incf open-count)))))
                     (puthash socket (plist-get arguments :on-message) callbacks)
                     (setq sockets (append sockets (list socket)))
                     socket)))
                ((symbol-function 'websocket-send-text)
                 (lambda (socket text)
                   (dolist (prompt '("fail independently"
                                     "complete independently"))
                     (when (string-match-p (regexp-quote prompt) text)
                       (puthash prompt socket socket-by-prompt)))
                   t))
                ((symbol-function 'websocket-close) (lambda (&rest _) t)))
        (e-provider-continuation-integration--start-session
         harness "session-one" "fail independently")
        (e-provider-continuation-integration--start-session
         harness "session-two" "complete independently")
        (should
         (e-provider-continuation-integration--wait-until
          (lambda () (= open-count 2))))
        (funcall
         (gethash (gethash "fail independently" socket-by-prompt) callbacks)
         (gethash "fail independently" socket-by-prompt)
         (json-encode
          '(:type "response.failed"
            :response
            (:status "failed"
             :error (:code "context_length_exceeded"
                     :message "request is too large")))))
        (should
         (e-provider-continuation-integration--wait-until
          (lambda ()
            (e-provider-continuation-integration--terminal-type-p
             (gethash "session-one" events) 'turn-failed))))
        (should (e-chat-service-active-turn-p harness "session-two"))
        (should-not
         (e-provider-continuation-integration--terminal-type-p
          (gethash "session-two" events) 'turn-failed))
        (e-provider-continuation-integration--finish-socket
         (gethash "complete independently" socket-by-prompt)
         callbacks "response-two" "survived error")
        (should
         (e-provider-continuation-integration--wait-until
          (lambda ()
            (e-provider-continuation-integration--terminal-type-p
             (gethash "session-two" events) 'turn-finished))))))))

(provide 'e-provider-continuation-integration-test)

;;; e-provider-continuation-integration-test.el ends here

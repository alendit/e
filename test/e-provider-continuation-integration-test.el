;;; e-provider-continuation-integration-test.el --- Provider continuation integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Deterministic integration tests for provider continuation across the harness,
;; OpenAI adapter, tool execution, and follow-up request construction.

;;; Code:

(require 'ert)
(require 'json)
(require 'e)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-openai)
(require 'e-tools)
(load (expand-file-name
       "../e2e/e-board-e2e-support.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil nil t)

(defun e-provider-continuation-integration--sse (&rest events)
  "Return an SSE stream containing JSON EVENTS."
  (mapconcat
   (lambda (event)
     (format "data: %s\n\n" (json-encode event)))
   events
   ""))

(defun e-provider-continuation-integration--input-types (body)
  "Return the Responses input item types from JSON BODY."
  (mapcar (lambda (item) (alist-get 'type item))
          (alist-get 'input body)))

(defun e-provider-continuation-integration--first-input-text (body)
  "Return the first input message text from JSON BODY."
  (alist-get
   'text
   (aref (alist-get 'content (aref (alist-get 'input body) 0)) 0)))

(ert-deftest e-provider-continuation-integration-test-tool-followup-advances-anchor ()
  "An anchored tool turn advances to its response and sends only tool output."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-openai-model-providers
          '((continuation-e2e
             :name "Continuation E2E"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :response-store t
             :continuation t
             :requires-openai-auth nil)))
         (requests nil)
         (call-count 0)
         (harness
          (e-openai-create-harness
           :provider 'continuation-e2e
           :model "gpt-test"
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers)
              (setq call-count (1+ call-count))
              (push (json-read-from-string body) requests)
              (pcase call-count
                (1
                 (e-provider-continuation-integration--sse
                  '((type . "response.output_text.done")
                    (text . "seed answer"))
                  '((type . "response.completed")
                    (response . ((id . "resp-seed")
                                 (status . "completed"))))))
                (2
                 (e-provider-continuation-integration--sse
                  '((type . "response.output_item.done")
                    (item . ((type . "function_call")
                             (call_id . "call-1")
                             (name . "inspect")
                             (arguments . "{\"target\":\"state\"}"))))
                  '((type . "response.completed")
                    (response . ((id . "resp-tool")
                                 (status . "completed"))))))
                (_
                 (e-provider-continuation-integration--sse
                  '((type . "response.output_text.done")
                    (text . "final answer"))
                  '((type . "response.completed")
                    (response . ((id . "resp-final")
                                 (status . "completed"))))))))))))
    (e-harness-activate-capability
     harness
     (e-capability-create
      :id 'inspect-capability
      :tools
      (list
       (lambda (registry)
         (e-tools-register
          registry
          :name "inspect"
          :description "Inspect state."
          :work
          (e-tools-cheap-work
           "e2e.provider-continuation.inspect"
           (lambda (arguments)
             (format "fresh %s"
                     (plist-get arguments :target)))))))))
    (e-board-e2e-create-session harness :id "session-1")
    (e-board-e2e-prompt-batch harness "session-1" "seed")
    (e-board-e2e-prompt-batch harness "session-1" "inspect now")
    (let* ((ordered (nreverse requests))
           (anchored-tool-request (nth 1 ordered))
           (followup-request (nth 2 ordered)))
      (should (= call-count 3))
      (should (equal (alist-get 'previous_response_id anchored-tool-request)
                     "resp-seed"))
      (should (equal (e-provider-continuation-integration--input-types
                      anchored-tool-request)
                     '("message")))
      (should (equal (e-provider-continuation-integration--first-input-text
                      anchored-tool-request)
                     "inspect now"))
      (should (equal (alist-get 'previous_response_id followup-request)
                     "resp-tool"))
      (should (equal (e-provider-continuation-integration--input-types
                      followup-request)
                     '("function_call_output")))
      (let* ((input (alist-get 'input followup-request))
             (function-output (aref input 0)))
        (should (equal (alist-get 'call_id function-output) "call-1"))
        (should (equal (alist-get 'output function-output)
                       "fresh state"))))))

(ert-deftest e-provider-continuation-integration-test-fresh-turn-advances-each-tool-response ()
  "A fresh multi-tool turn chains each stored response without replay."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-openai-model-providers
          '((continuation-e2e
             :name "Continuation E2E"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :response-store t
             :continuation t
             :requires-openai-auth nil)))
         (requests nil)
         (call-count 0)
         (harness
          (e-openai-create-harness
           :provider 'continuation-e2e
           :model "gpt-test"
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers)
              (setq call-count (1+ call-count))
              (push (json-read-from-string body) requests)
              (if (< call-count 3)
                  (e-provider-continuation-integration--sse
                   `((type . "response.output_item.done")
                     (item . ((type . "function_call")
                              (call_id . ,(format "call-%d" call-count))
                              (name . "inspect")
                              (arguments . ,(format
                                             "{\"target\":\"state-%d\"}"
                                             call-count)))))
                   `((type . "response.completed")
                     (response . ((id . ,(format "resp-tool-%d" call-count))
                                  (status . "completed")))))
                (e-provider-continuation-integration--sse
                 '((type . "response.output_text.done")
                   (text . "final answer"))
                 '((type . "response.completed")
                   (response . ((id . "resp-final")
                                (status . "completed")))))))))))
    (e-harness-activate-capability
     harness
     (e-capability-create
      :id 'inspect-capability
      :tools
      (list
       (lambda (registry)
         (e-tools-register
          registry
          :name "inspect"
          :description "Inspect state."
          :work
          (e-tools-cheap-work
           "e2e.provider-continuation.inspect"
           (lambda (arguments)
             (format "fresh %s" (plist-get arguments :target)))))))))
    (e-board-e2e-create-session harness :id "session-1")
    (e-board-e2e-prompt-batch harness "session-1" "inspect twice")
    (let ((ordered (nreverse requests)))
      (should (= call-count 3))
      (should-not (alist-get 'previous_response_id (nth 0 ordered)))
      (should (equal (alist-get 'previous_response_id (nth 1 ordered))
                     "resp-tool-1"))
      (should (equal (e-provider-continuation-integration--input-types
                      (nth 1 ordered))
                     '("function_call_output")))
      (should (equal (alist-get 'previous_response_id (nth 2 ordered))
                     "resp-tool-2"))
      (should (equal (e-provider-continuation-integration--input-types
                      (nth 2 ordered))
                     '("function_call_output"))))))

(ert-deftest e-provider-continuation-integration-test-websocket-isolated-by-session ()
  "Concurrent harness sessions do not share one active Responses request."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-openai-websocket-idle-timeout-seconds nil)
         (e-openai-model-providers
          '((continuation-websocket-e2e
             :name "Continuation WebSocket E2E"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :response-store t
             :continuation t
             :requires-openai-auth nil)))
         (harness
          (e-openai-create-harness
           :provider 'continuation-websocket-e2e
           :model "gpt-test"))
         (open-count 0)
         (callbacks (make-hash-table :test 'eq))
         sockets)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (let ((socket (intern (format "fake-websocket-%d"
                                               (cl-incf open-count)))))
                   (puthash socket (plist-get args :on-message) callbacks)
                   (push socket sockets)
                   socket)))
              ((symbol-function 'websocket-send-text)
               (lambda (_websocket _text) t))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (e-board-e2e-reset-runtime)
      (e-chat-service-create-session :harness harness :id "session-one")
      (e-chat-service-create-session :harness harness :id "session-two")
      (e-board-e2e-prompt-async harness "session-one" "first request")
      (e-board-e2e-prompt-async harness "session-two" "second request")
      (should (= open-count 2))
      (dolist (socket sockets)
        (let ((on-message (gethash socket callbacks)))
          (funcall on-message socket
                   (json-encode
                    '(:type "response.output_text.done" :text "done")))
          (funcall on-message socket
                   (json-encode
                    `(:type "response.completed"
                      :response (:id ,(format "response-%s" socket)
                                 :status "completed"))))))
      (should (eq (plist-get (e-board-e2e-wait-batch
                              harness "session-one" 1.0)
                             :status)
                  'done))
      (should (eq (plist-get (e-board-e2e-wait-batch
                              harness "session-two" 1.0)
                             :status)
                  'done)))))

(ert-deftest e-provider-continuation-integration-test-websocket-failure-retry-clears-active-request ()
  "A terminal Responses failure releases the socket before harness retry."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-harness-retry-initial-backoff-seconds 0.01)
         (e-harness-retry-backoff-multiplier 1.0)
         (e-harness-retry-max-backoff-seconds 0.01)
         (e-harness-retry-max-elapsed-seconds 1.0)
         (e-openai-websocket-idle-timeout-seconds nil)
         (e-openai-model-providers
          '((continuation-websocket-e2e
             :name "Continuation WebSocket E2E"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :response-store t
             :continuation t
             :requires-openai-auth nil)))
         (harness
          (e-openai-create-harness
           :provider 'continuation-websocket-e2e
           :model "gpt-test"))
         (open-count 0)
         (send-count 0)
         (events nil)
         (callbacks (make-hash-table :test 'eq)))
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (let ((socket (intern (format "fake-websocket-%d"
                                               (cl-incf open-count)))))
                   (puthash socket (plist-get args :on-message) callbacks)
                   socket)))
              ((symbol-function 'websocket-send-text)
               (lambda (socket _text)
                 (cl-incf send-count)
                 (let ((on-message (gethash socket callbacks)))
                   (if (= send-count 1)
                       (funcall
                        on-message socket
                        (json-encode
                         '(:type "response.failed"
                           :response
                           (:status "failed"
                            :error
                            (:code "server_error"
                             :message "529: overloaded_error")))))
                     (funcall on-message socket
                              (json-encode
                               '(:type "response.output_text.done"
                                 :text "recovered")))
                     (funcall on-message socket
                              (json-encode
                               '(:type "response.completed"
                                 :response (:id "response-recovered"
                                            :status "completed"))))))))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (e-board-e2e-reset-runtime)
      (e-chat-service-create-session :harness harness :id "session-one")
      (e-harness--install-activity-sink
       harness (lambda (event) (push event events)) :session-id "session-one")
      (e-board-e2e-prompt-async harness "session-one" "recover once")
      (let ((result (e-board-e2e-wait-batch harness "session-one" 1.0)))
        (should (eq (plist-get result :status) 'done)))
      (should (= send-count 2))
      (should (= open-count 2))
      (let ((types (mapcar (lambda (event) (plist-get event :type)) events)))
        (should (= (seq-count (lambda (type) (eq type 'turn-retrying)) types)
                   1))
        (should-not (memq 'turn-failed types))))))

(provide 'e-provider-continuation-integration-test)

;;; e-provider-continuation-integration-test.el ends here

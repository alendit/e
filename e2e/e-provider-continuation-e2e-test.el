;;; e-provider-continuation-e2e-test.el --- Provider continuation e2e tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Deterministic end-to-end tests for provider continuation across the harness,
;; OpenAI adapter, tool execution, and follow-up request construction.

;;; Code:

(require 'ert)
(require 'json)
(require 'e)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-openai)
(require 'e-tools)

(defun e-provider-continuation-e2e--sse (&rest events)
  "Return an SSE stream containing JSON EVENTS."
  (mapconcat
   (lambda (event)
     (format "data: %s\n\n" (json-encode event)))
   events
   ""))

(defun e-provider-continuation-e2e--input-types (body)
  "Return the Responses input item types from JSON BODY."
  (mapcar (lambda (item) (alist-get 'type item))
          (alist-get 'input body)))

(defun e-provider-continuation-e2e--first-input-text (body)
  "Return the first input message text from JSON BODY."
  (alist-get
   'text
   (aref (alist-get 'content (aref (alist-get 'input body) 0)) 0)))

(ert-deftest e-provider-continuation-e2e-test-tool-followup-sends-in-turn-output ()
  "A full anchored tool turn sends function call and output on follow-up."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
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
                 (e-provider-continuation-e2e--sse
                  '((type . "response.output_text.done")
                    (text . "seed answer"))
                  '((type . "response.completed")
                    (response . ((id . "resp-seed")
                                 (status . "completed"))))))
                (2
                 (e-provider-continuation-e2e--sse
                  '((type . "response.output_item.done")
                    (item . ((type . "function_call")
                             (call_id . "call-1")
                             (name . "inspect")
                             (arguments . "{\"target\":\"state\"}"))))
                  '((type . "response.completed")
                    (response . ((id . "resp-tool")
                                 (status . "completed"))))))
                (_
                 (e-provider-continuation-e2e--sse
                  '((type . "response.output_text.done")
                    (text . "final answer"))
                  '((type . "response.completed")
                    (response . ((id . "resp-final")
                                 (status . "completed"))))))))))))
    (e-harness-create-session harness :id "session-1")
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
          :handler (lambda (arguments)
                     (format "fresh %s"
                             (plist-get arguments :target))))))))
    (e-harness-prompt-batch harness "session-1" "seed")
    (e-harness-prompt-batch harness "session-1" "inspect now")
    (let* ((ordered (nreverse requests))
           (anchored-tool-request (nth 1 ordered))
           (followup-request (nth 2 ordered)))
      (should (= call-count 3))
      (should (equal (alist-get 'previous_response_id anchored-tool-request)
                     "resp-seed"))
      (should (equal (e-provider-continuation-e2e--input-types
                      anchored-tool-request)
                     '("message")))
      (should (equal (e-provider-continuation-e2e--first-input-text
                      anchored-tool-request)
                     "inspect now"))
      (should (equal (alist-get 'previous_response_id followup-request)
                     "resp-seed"))
      (should (equal (e-provider-continuation-e2e--input-types
                      followup-request)
                     '("message" "function_call" "function_call_output")))
      (let* ((input (alist-get 'input followup-request))
             (function-call (aref input 1))
             (function-output (aref input 2)))
        (should (equal (alist-get 'call_id function-call) "call-1"))
        (should (equal (alist-get 'call_id function-output) "call-1"))
        (should (equal (alist-get 'output function-output)
                       "fresh state"))))))

(provide 'e-provider-continuation-e2e-test)

;;; e-provider-continuation-e2e-test.el ends here

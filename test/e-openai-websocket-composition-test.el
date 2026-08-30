;;; e-openai-websocket-composition-test.el --- Tests for e OpenAI/Codex backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for OpenAI WebSocket continuation and lifecycle composition.

;;; Code:

(require 'ert)
(require 'json)
(require 'e)
(require 'e-backend)
(require 'e-dev-profile)
(require 'e-harness)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-loop)
(require 'e-openai)
(require 'url-http)

(load (expand-file-name "e-openai-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-openai-test-request-body-websocket-defaults-to-implicit-store ()
  "Responses WebSocket requests default to stored responses without serializing true."
  (let ((body (e-openai-codex-request-body
               :messages '((:role user :content "hello"))
               :options '(:model "gpt-test"
                          :responses-transport websocket))))
    (should-not (plist-member body :store))
    (should-not (plist-member body :stream))))

(ert-deftest e-openai-test-request-body-websocket-store-true-is-implicit ()
  "Responses WebSocket store=true uses the stored default wire shape."
  (let ((body (e-openai-codex-request-body
               :messages '((:role user :content "hello"))
               :options '(:model "gpt-test"
                          :responses-transport websocket
                          :response-store t))))
    (should-not (plist-member body :store))
    (should-not (plist-member body :stream))))

(ert-deftest e-openai-test-request-body-response-store-overrides-websocket-default ()
  "Explicit response-store config overrides the WebSocket store default."
  (let ((body (e-openai-codex-request-body
               :messages '((:role user :content "hello"))
               :options '(:model "gpt-test"
                          :responses-transport websocket
                          :response-store :json-false
                          :provider-continuation t))))
    (should (eq (plist-get body :store) :json-false))
    (should-not (plist-member body :stream))))

(ert-deftest e-openai-test-request-body-websocket-store-false-uses-local-anchor ()
  "Unstored WebSocket requests can use the active connection's response anchor."
  (let ((body (e-openai-codex-request-body
               :messages '((:role system :content "current instructions")
                           (:role user :content "old prompt")
                           (:role assistant :content "old answer")
                           (:role user :content "new prompt"))
               :options '(:model "gpt-test"
                          :responses-transport websocket
                          :response-store :json-false
                          :provider-continuation t
                          :provider-anchor (:provider-id openai
                                            :metadata (:response-id "resp-1"
                                                       :reasoning-identity
                                                       (:effort "high" :summary "auto")))
                          :provider-anchor-delta-messages
                          ((:role user :content "new prompt"))))))
    (should (eq (plist-get body :store) :json-false))
    (should (equal (plist-get body :previous_response_id) "resp-1"))
    (should (equal (plist-get body :input)
                   [(:type "message"
                     :role "user"
                     :content [(:type "input_text" :text "new prompt")])]))))

(ert-deftest e-openai-test-default-harness-uses-codex-websocket-continuation ()
  "The built-in Codex profile uses connection-local WebSocket continuation."
  (should (equal (e-harness-default-options
                  (e-openai-create-harness :request-function #'ignore))
                 '(:model "gpt-5.5"
                   :reasoning-effort "high"
                   :reasoning-summary "auto"
                   :provider-continuation t
                   :provider-anchor-provider-id openai))))

(ert-deftest e-openai-test-request-context-rejects-negative-websocket-idle-policy-before-request ()
  "A negative explicit policy fails before opening a WebSocket."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token"
                process-environment))
         (e-openai-model-providers
          '((websocket-policy
             :name "WebSocket Policy"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :websocket-idle-close-seconds -1
             :requires-openai-auth nil)))
         (backend (e-openai-backend-create :provider 'websocket-policy))
         (websocket-opened nil))
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (&rest _args)
                 (setq websocket-opened t)
                 'fake-websocket)))
      (should-error
       (e-backend-start backend
                       :messages '((:role user :content "hello"))
                       :options '(:model "gpt-test")
                       :on-item #'ignore
                       :on-done #'ignore
                       :on-error #'ignore)
       :type 'e-openai-provider-invalid))
    (should-not websocket-opened)))

(ert-deftest e-openai-test-backend-streams-through-injected-requester ()
  "The Codex backend streams parsed events from an injected HTTP requester."
  (let* ((token (e-openai-test--jwt))
         (auth-file (make-temp-file "e-auth" nil ".json"
                                    (json-encode
                                     (list :tokens
                                           (list :access_token token
                                                 :refresh_token "refresh")))))
         (e-openai-model-providers
          `((codex
             :name "ChatGPT Codex HTTP"
             :base-url ,(concat e-openai-codex-default-base-url "/codex")
             :wire-api responses
             :responses-transport http
             :requires-openai-auth t)))
         (seen nil)
         (captured nil)
         (backend
          (e-openai-codex-backend-create
           :auth-file auth-file
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (setq captured (list :url url :headers headers :body body))
              "data: {\"type\":\"response.output_text.done\",\"text\":\"ok\"}\n\n\
data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")))))
    (unwind-protect
        (progn
          (e-backend-stream-batch backend
                            :messages '((:role user :content "hello"))
                            :options '(:model "gpt-test")
                            :on-item (lambda (item) (push item seen)))
          (should (equal (nreverse seen)
                         '((:type assistant-message :content "ok")
                           (:type done :reason stop))))
          (should (equal (plist-get captured :url)
                         "https://chatgpt.com/backend-api/codex/responses"))
          (should (assoc "Authorization" (plist-get captured :headers)))
          (should (assoc "chatgpt-account-id" (plist-get captured :headers))))
      (delete-file auth-file))))

(ert-deftest e-openai-test-websocket-backend-streams-response-events ()
  "Responses WebSocket profiles stream JSON events through backend callbacks."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :requires-openai-auth nil)))
         opened-url opened-headers sent on-message done-status error seen request)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (url &rest args)
                 (setq opened-url url)
                 (setq opened-headers (plist-get args :custom-header-alist))
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket text)
                 (should (eq websocket 'fake-websocket))
                 (setq sent (json-parse-string text
                                               :object-type 'plist
                                               :array-type 'list
                                               :null-object nil
                                               :false-object :json-false))
                 (funcall on-message
                          websocket
                          (json-encode
                           '(:type "response.output_text.delta"
                             :delta "ok")))
                 (funcall on-message
                          websocket
                          (json-encode
                           '(:type "response.completed"
                             :response (:id "resp-ws-1"
                                        :status "completed"))))))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (setq request
              (e-backend-start backend
                               :messages '((:role user :content "hello"))
                               :options '(:model "gpt-test")
                               :on-item (lambda (item) (push item seen))
                               :on-done (lambda (status)
                                          (setq done-status status))
                               :on-error (lambda (err) (setq error err))))
        (should (e-openai-test--wait-until (lambda () done-status) 0.2))
        (should-not error)
        (should (e-backend-request-p request))
        (should (equal opened-url "wss://gateway.example.test/v1/responses"))
        (should (equal (cdr (assoc "Authorization" opened-headers))
                       "Bearer test-gateway-token"))
        (should (equal (cdr (assoc "OpenAI-Beta" opened-headers))
                       "responses_websockets=2026-02-06"))
        (should (equal (plist-get sent :type) "response.create"))
        (should-not (plist-member sent :store))
        (should-not (plist-member sent :stream))
        (should (equal (nreverse seen)
                       '((:type assistant-delta :content "ok")
                         (:type provider-anchor-candidate
                          :provider-id openai
                          :metadata (:response-id "resp-ws-1"
                                    :reasoning-identity
                                    (:effort "high" :summary "auto")))
                         (:type done :reason stop))))
        (should (eq (plist-get (e-backend-request-metadata request)
                               :transport)
                    'websocket))))))

(ert-deftest e-openai-test-websocket-buffers-assistant-message-candidates ()
  "Responses WebSocket internals do not leak assistant-message-candidate items."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :requires-openai-auth nil)))
         on-message done-status error seen)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket _text)
                 (funcall on-message
                          websocket
                          (json-encode
                           '(:type "response.content_part.done"
                             :part (:type "output_text"
                                    :text "candidate answer"))))
                 (funcall on-message
                          websocket
                          (json-encode
                           '(:type "response.output_item.done"
                             :item (:type "message"
                                    :content
                                    [(:type "output_text"
                                      :text "candidate answer")]))))
                 (funcall on-message
                          websocket
                          (json-encode
                           '(:type "response.completed"
                             :response (:id "resp-ws-1"
                                        :status "completed"))))))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start backend
                         :messages '((:role user :content "hello"))
                         :options '(:model "gpt-test")
                         :on-item (lambda (item) (push item seen))
                         :on-done (lambda (status)
                                    (setq done-status status))
                         :on-error (lambda (err) (setq error err)))
        (should (e-openai-test--wait-until (lambda () done-status) 0.2))
        (should-not error)
        (should (equal (nreverse seen)
                       '((:type provider-anchor-candidate
                          :provider-id openai
                          :metadata (:response-id "resp-ws-1"
                                    :reasoning-identity
                                    (:effort "high" :summary "auto")))
                         (:type assistant-message
                          :content "candidate answer")
                         (:type done :reason stop))))))))

(ert-deftest e-openai-test-websocket-does-not-use-http-request-timeout ()
  "Responses WebSocket requests do not inherit the HTTP whole-request timeout."
  (let* ((e-openai-request-timeout-seconds 0.01)
         (e-openai-websocket-idle-timeout-seconds nil)
         (process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :requires-openai-auth nil)))
         on-message done-status error seen)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (&rest _args) t))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start backend
                         :messages '((:role user :content "hello"))
                         :options '(:model "gpt-test")
                         :on-item (lambda (item) (push item seen))
                         :on-done (lambda (status)
                                    (setq done-status status))
                         :on-error (lambda (err) (setq error err)))
        (accept-process-output nil 0.05)
        (should-not error)
        (should-not done-status)
        (funcall on-message
                 'fake-websocket
                 (json-encode
                  '(:type "response.completed"
                    :response (:id "resp-ws-1" :status "completed"))))
        (should (e-openai-test--wait-until (lambda () done-status) 0.2))
        (should-not error)
        (should (equal (nreverse seen)
                       '((:type provider-anchor-candidate
                          :provider-id openai
                          :metadata (:response-id "resp-ws-1"
                                    :reasoning-identity
                                    (:effort "high" :summary "auto")))
                         (:type done :reason stop))))))))

(ert-deftest e-openai-test-websocket-completion-retains-connection ()
  "Responses WebSocket completion retains the transport for reuse."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :requires-openai-auth nil)))
         on-message done-status error late-seen request (close-count 0))
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket _text)
                 (funcall on-message
                          websocket
                          (json-encode
                           '(:type "response.completed"
                             :response (:id "resp-ws-1"
                                        :status "completed"))))
                 (funcall on-message
                          websocket
                          (json-encode
                           '(:type "response.output_text.delta"
                             :delta "late")))))
              ((symbol-function 'websocket-close)
               (lambda (&rest _args)
                 (cl-incf close-count)
                 t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (setq request
              (e-backend-start backend
                               :messages '((:role user :content "hello"))
                               :options '(:model "gpt-test")
                               :on-item (lambda (item) (push item late-seen))
	                       :on-done (lambda (status)
	                                  (setq done-status status))
	                       :on-error (lambda (err) (setq error err))))
        (should (e-openai-test--wait-until (lambda () done-status) 0.2))
        (should-not error)
        (should (= close-count 0))
        (should (equal (nreverse late-seen)
                       '((:type provider-anchor-candidate
                          :provider-id openai
                          :metadata (:response-id "resp-ws-1"
                                    :reasoning-identity
                                    (:effort "high" :summary "auto")))
                         (:type done :reason stop))))
        (should (e-backend-cancel-request request))
        (should (= close-count 1))))))

(ert-deftest e-openai-test-websocket-reuses-compatible-session-connection ()
  "Compatible follow-ups reuse one connection and send incremental input."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :continuation t
             :requires-openai-auth nil)))
         (open-count 0)
         (close-count 0)
         sends
         on-message
         (done-count 0)
         second-request)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (cl-incf open-count)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket text)
                 (let* ((payload
                         (json-parse-string text
                                            :object-type 'plist
                                            :array-type 'list
                                            :null-object nil
                                            :false-object :json-false))
                        (response-id
                         (if (null sends) "resp-one" "resp-two")))
                   (push payload sends)
                   (funcall on-message
                            websocket
                            (json-encode
                             `(:type "response.completed"
                               :response (:id ,response-id
                                          :status "completed")))))))
              ((symbol-function 'websocket-close)
               (lambda (&rest _args)
                 (cl-incf close-count)
                 t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start
         backend
         :messages '((:role user :content "one"))
         :options '(:model "gpt-test"
                    :session-id "session-one"
                    :provider-continuation t)
         :on-item #'ignore
         :on-done (lambda (_status) (cl-incf done-count))
         :on-error #'signal)
        (should (e-openai-test--wait-until (lambda () (= done-count 1)) 0.2))
        (setq second-request
              (e-backend-start
               backend
               :messages '((:role user :content "one"))
               :options
               '(:model "gpt-test"
                 :session-id "session-one"
                 :provider-continuation t
                 :provider-anchor
                 (:provider-id openai
                  :metadata (:response-id "resp-one"
                             :reasoning-identity
                             (:effort "high" :summary "auto")))
                 :provider-anchor-delta-messages
                 ((:role tool
                   :content (:tool-call-id "call-one" :content "result")))
                 :provider-anchor-source-message-count 1)
               :on-item #'ignore
               :on-done (lambda (_status) (cl-incf done-count))
               :on-error #'signal))
        (should (e-openai-test--wait-until (lambda () (= done-count 2)) 0.2))
        (should (= open-count 1))
        (should (= close-count 0))
        (let* ((chronological (nreverse sends))
               (second (cadr chronological))
               (diagnostics
                (plist-get (e-backend-request-metadata second-request)
                           :diagnostics)))
          (should (equal (plist-get second :previous_response_id) "resp-one"))
          (should (equal (mapcar (lambda (item) (plist-get item :type))
                                 (plist-get second :input))
                         '("function_call_output")))
          (should (eq (plist-get diagnostics :websocket-reused) t))
          (should (eq (plist-get diagnostics :websocket-request-mode)
                      'incremental))
          (should (= (plist-get diagnostics :websocket-reuse-count) 1)))
        (e-backend-cancel-request second-request)
        (should (= close-count 1))))))

(ert-deftest e-openai-test-websocket-changed-instructions-continue-incrementally ()
  "Replacement instructions keep the current response anchor and socket."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :continuation t
             :requires-openai-auth nil)))
         (open-count 0)
         sends
         on-message
         (done-count 0)
         second-request)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (cl-incf open-count)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket text)
                 (let ((payload
                        (json-parse-string text
                                           :object-type 'plist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object :json-false)))
                   (push payload sends)
                   (funcall on-message
                            websocket
                            (json-encode
                             `(:type "response.completed"
                               :response
                               (:id ,(if (= (length sends) 1)
                                         "resp-one"
                                       "resp-two")
                                :status "completed")))))))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start
         backend
         :messages '((:role system :content "instruction one")
                     (:role user :content "one"))
         :options '(:model "gpt-test" :session-id "session-one")
         :on-item #'ignore
         :on-done (lambda (_status) (cl-incf done-count))
         :on-error #'signal)
        (should (e-openai-test--wait-until (lambda () (= done-count 1)) 0.2))
        (setq second-request
              (e-backend-start
               backend
               :messages '((:role system :content "instruction two")
                           (:role user :content "two"))
               :options
               '(:model "gpt-test"
                 :session-id "session-one"
                 :provider-continuation t
                 :provider-anchor
                 (:provider-id openai
                  :metadata (:response-id "resp-one"
                             :reasoning-identity
                             (:effort "high" :summary "auto")))
                 :provider-anchor-delta-messages
                 ((:role user :content "two"))
                 :provider-anchor-source-message-count 2)
               :on-item #'ignore
               :on-done (lambda (_status) (cl-incf done-count))
               :on-error #'signal))
        (should (e-openai-test--wait-until (lambda () (= done-count 2)) 0.2))
        (should (= open-count 1))
        (let* ((second (car sends))
               (diagnostics
                (plist-get (e-backend-request-metadata second-request)
                           :diagnostics)))
          (should (equal (plist-get second :previous_response_id) "resp-one"))
          (should (equal (mapcar (lambda (item) (plist-get item :type))
                                 (plist-get second :input))
                         '("message")))
          (should (equal (plist-get second :instructions)
                         "You are a helpful assistant.\n\ninstruction two"))
          (should (equal (plist-get diagnostics :websocket-request-mode)
                         'incremental)))
        (e-backend-cancel-request second-request)))))

(ert-deftest e-openai-test-websocket-reconnects-with-full-request ()
  "A follow-up after server close reconnects without the stale response id."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :continuation t
             :requires-openai-auth nil)))
         (open-count 0)
         sends
         on-message
         on-close
         current-websocket
         (done-count 0)
         second-request)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (cl-incf open-count)
                 (setq on-message (plist-get args :on-message))
                 (setq on-close (plist-get args :on-close))
                 (setq current-websocket
                       (intern (format "fake-websocket-%d" open-count)))))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket text)
                 (let ((payload
                        (json-parse-string text
                                           :object-type 'plist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object :json-false)))
                   (push payload sends)
                   (funcall on-message
                            websocket
                            (json-encode
                             `(:type "response.completed"
                               :response
                               (:id ,(if (= (length sends) 1)
                                         "resp-one"
                                       "resp-two")
                                :status "completed")))))))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start
         backend
         :messages '((:role user :content "one"))
         :options '(:model "gpt-test" :session-id "session-one")
         :on-item #'ignore
         :on-done (lambda (_status) (cl-incf done-count))
         :on-error #'signal)
        (should (e-openai-test--wait-until (lambda () (= done-count 1)) 0.2))
        (funcall on-close current-websocket)
        (setq second-request
              (e-backend-start
               backend
               :messages '((:role user :content "one")
                           (:role user :content "two"))
               :options
               '(:model "gpt-test"
                 :session-id "session-one"
                 :provider-continuation t
                 :provider-anchor
                 (:provider-id openai
                  :metadata (:response-id "resp-one"
                             :reasoning-identity
                             (:effort "high" :summary "auto")))
                 :provider-anchor-delta-messages
                 ((:role user :content "two"))
                 :provider-anchor-source-message-count 2)
               :on-item #'ignore
               :on-done (lambda (_status) (cl-incf done-count))
               :on-error #'signal))
        (should (e-openai-test--wait-until (lambda () (= done-count 2)) 0.2))
        (should (= open-count 2))
        (let* ((second (car sends))
               (diagnostics
                (plist-get (e-backend-request-metadata second-request)
                           :diagnostics)))
          (should-not (plist-member second :previous_response_id))
          (should (= (length (plist-get second :input)) 2))
          (should (eq (plist-get diagnostics :websocket-request-mode) 'full)))
        (e-backend-cancel-request second-request)))))

(ert-deftest e-openai-test-websocket-unresolved-anchor-retries-full-request ()
  "An unavailable connection-local response id retries once with full input."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :continuation t
             :requires-openai-auth nil)))
         sends
         on-message
         (done-count 0)
         error
         second-request
         third-request
         fourth-request)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket text)
                 (let ((payload
                        (json-parse-string text
                                           :object-type 'plist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object :json-false)))
                   (push payload sends)
                   (pcase (length sends)
                     (1
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.completed"
                                  :response (:id "resp-one"
                                             :status "completed")))))
                     (2
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.failed"
                                  :response
                                  (:error
                                   (:code "response_not_found"
                                    :param "previous_response_id"
                                    :message "previous_response_id not cached"))))))
                     (3
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.completed"
                                  :response (:id "resp-two"
                                             :status "completed")))))
                     (4
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.completed"
                                  :response (:id "resp-three"
                                             :status "completed")))))
                     (5
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.completed"
                                  :response (:id "resp-four"
                                             :status "completed")))))))))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start
         backend
         :messages '((:role user :content "one"))
         :options '(:model "gpt-test" :session-id "session-one")
         :on-item #'ignore
         :on-done (lambda (_status) (cl-incf done-count))
         :on-error #'signal)
        (should (e-openai-test--wait-until (lambda () (= done-count 1)) 0.2))
        (setq second-request
              (e-backend-start
               backend
               :messages '((:role user :content "one")
                           (:role user :content "two"))
               :options
               '(:model "gpt-test"
                 :session-id "session-one"
                 :provider-continuation t
                 :provider-anchor
                 (:provider-id openai
                  :metadata (:response-id "resp-one"
                             :reasoning-identity
                             (:effort "high" :summary "auto")))
                 :provider-anchor-delta-messages
                 ((:role user :content "two"))
                 :provider-anchor-source-message-count 2)
               :on-item #'ignore
               :on-done (lambda (_status) (cl-incf done-count))
               :on-error #'signal))
        (should (e-openai-test--wait-until (lambda () (= done-count 2)) 0.2))
        (let* ((chronological (reverse sends))
               (incremental (cadr chronological))
               (fallback (caddr chronological))
               (diagnostics
                (plist-get (e-backend-request-metadata second-request)
                           :diagnostics)))
          (should (equal (plist-get incremental :previous_response_id)
                         "resp-one"))
          (should-not (plist-member fallback :previous_response_id))
          (should (= (length (plist-get fallback :input)) 2))
          (should (eq (plist-get diagnostics :websocket-request-mode)
                      'full-retry)))
        (setq third-request
              (e-backend-start
               backend
               :messages '((:role user :content "one")
                           (:role user :content "two")
                           (:role user :content "three"))
               :options
               '(:model "gpt-test"
                 :session-id "session-one"
                 :provider-continuation t
                 :provider-anchor
                 (:provider-id openai
                  :metadata (:response-id "resp-one"
                             :reasoning-identity
                             (:effort "high" :summary "auto")))
                 :provider-anchor-delta-messages
                 ((:role user :content "three"))
                 :provider-anchor-source-message-count 2)
               :on-item #'ignore
               :on-done (lambda (_status) (cl-incf done-count))
               :on-error (lambda (err) (setq error err))))
        (should (e-openai-test--wait-until
                 (lambda () (or error (= done-count 3)))
                 0.2))
        (should-not error)
        (setq fourth-request
              (e-backend-start
               backend
               :messages '((:role user :content "one")
                           (:role user :content "two")
                           (:role user :content "three")
                           (:role user :content "four"))
               :options
               '(:model "gpt-test"
                 :session-id "session-one"
                 :provider-continuation t
                 :provider-anchor
                 (:provider-id openai
                  :metadata (:response-id "resp-two"
                             :reasoning-identity
                             (:effort "high" :summary "auto")))
                 :provider-anchor-delta-messages
                 ((:role user :content "three")
                  (:role user :content "four"))
                 :provider-anchor-source-message-count 3)
               :on-item #'ignore
               :on-done (lambda (_status) (cl-incf done-count))
               :on-error #'signal))
        (should (e-openai-test--wait-until (lambda () (= done-count 4)) 0.2))
        (let* ((chronological (nreverse sends))
               (removed-anchor-request (nth 3 chronological))
               (stale-anchor-request (nth 4 chronological))
               (removed-diagnostics
                (plist-get (e-backend-request-metadata third-request)
                           :diagnostics))
               (stale-diagnostics
                (plist-get (e-backend-request-metadata fourth-request)
                           :diagnostics)))
          (should-not (plist-member removed-anchor-request
                                      :previous_response_id))
          (should (eq (plist-get removed-diagnostics :websocket-request-mode)
                      'full))
          ;; Only the latest response (resp-three) is eligible.  The older
          ;; resp-two anchor is intentionally sent as a full request.
          (should-not (plist-member stale-anchor-request
                                      :previous_response_id))
          (should (eq (plist-get stale-diagnostics
                                 :websocket-request-mode)
                      'full)))
        (e-backend-cancel-request fourth-request)))))

(ert-deftest e-openai-test-websocket-unresolved-anchor-retries-one-canonical-bundle-for-both-error-forms ()
  "Both unavailable-response error forms get one complete canonical retry.

The second request is the matching immediate continuation and therefore carries
only its function-call output.  The adapter's one retry must remove the
unavailable response id and reconstruct the call, opaque reasoning replay, and
result from the canonical messages supplied by the caller."
  (dolist (error-form '("error" "response.failed"))
    (let* ((process-environment
            (cons "OPENAI_API_KEY=test-gateway-token" process-environment))
           (e-openai-model-providers
            '((openai-websocket
               :name "OpenAI WebSocket"
               :base-url "https://gateway.example.test/v1"
               :env-key "OPENAI_API_KEY"
               :wire-api responses
               :responses-transport websocket
               :continuation t
               :requires-openai-auth nil)))
           (sends nil)
           (on-message nil)
           (done-count 0)
           (failure nil)
           recovery-request)
      (cl-letf (((symbol-function 'websocket-open)
                 (lambda (_url &rest args)
                   (setq on-message (plist-get args :on-message))
                   'fake-websocket))
                ((symbol-function 'websocket-send-text)
                 (lambda (websocket text)
                   (let ((payload
                          (json-parse-string
                           text
                           :object-type 'plist
                           :array-type 'list
                           :null-object nil
                           :false-object :json-false)))
                     (push payload sends)
                     (pcase (length sends)
                       (1
                        (funcall on-message
                                 websocket
                                 (json-encode
                                  '(:type "response.completed"
                                    :response (:id "resp-latest"
                                               :status "completed")))))
                       (2
                        (funcall on-message
                                 websocket
                                 (json-encode
                                  (if (equal error-form "error")
                                      '(:type "error"
                                        :code "previous_response_not_found"
                                        :param "previous_response_id"
                                        :message "previous response not found")
                                    '(:type "response.failed"
                                      :response
                                      (:error
                                       (:code "previous_response_not_found"
                                        :param "previous_response_id"
                                        :message "previous response not found")))))))
                       (3
                        (funcall on-message
                                 websocket
                                 (json-encode
                                  '(:type "response.completed"
                                    :response (:id "resp-recovered"
                                               :status "completed")))))
                       (_ (error "Unexpected retry beyond one canonical recovery"))))))
                ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
        (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
          ;; Prime the connection-local response state so the next request is
          ;; a real immediate continuation rather than a new-connection full
          ;; request.
          (e-backend-start
           backend
           :messages '((:role user :content "seed"))
           :options '(:model "gpt-test" :session-id "session-one")
           :on-item #'ignore
           :on-done (lambda (_status) (cl-incf done-count))
           :on-error (lambda (err) (setq failure err)))
          (should (e-openai-test--wait-until
                   (lambda () (or failure (= done-count 1)))
                   0.2))
          (should-not failure)
          (setq failure nil)
          (setq recovery-request
                (e-backend-start
                 backend
                 :messages
                 '((:role user :content "request before tool")
                     (:role tool-call
                      :content (:id "call-latest"
                                :name "inspect"
                                :arguments (:target "raw")
                                :provider-replay-items
                                ((:provider-id openai
                                  :item (:type "reasoning"
                                         :id "replay-latest"
                                         :encrypted_content "RAW-REPLAY"
                                         :summary [])))))
                     (:role tool
                      :content (:tool-call-id "call-latest"
                                :name "inspect"
                                :content "RAW-RESULT")))
                 :options
                 '(:model "gpt-test"
                   :session-id "session-one"
                   :provider-continuation t
                   :provider-anchor
                   (:provider-id openai
                    :metadata (:response-id "resp-latest"
                               :reasoning-identity
                               (:effort "high" :summary "auto")))
                   :provider-anchor-delta-messages
                   ((:role tool
                     :content (:tool-call-id "call-latest"
                               :name "inspect"
                               :content "RAW-RESULT")))
                   :provider-anchor-source-message-count 3)
                 :on-item #'ignore
                 :on-done (lambda (_status) (cl-incf done-count))
                 :on-error (lambda (err) (setq failure err))))
          (should (e-openai-test--wait-until
                   (lambda () (or failure (= done-count 2)))
                   0.2))
          (let* ((chronological (reverse sends))
                 (incremental (nth 1 chronological))
                 (fallback (nth 2 chronological))
                 (fallback-input (plist-get fallback :input))
                 (replay
                  (seq-find
                   (lambda (item)
                     (equal (plist-get item :type) "reasoning"))
                   fallback-input))
                 (call
                  (seq-find
                   (lambda (item)
                     (and (equal (plist-get item :type) "function_call")
                          (equal (plist-get item :call_id) "call-latest")))
                   fallback-input))
                 (result
                  (seq-find
                   (lambda (item)
                     (and (equal (plist-get item :type)
                                 "function_call_output")
                          (equal (plist-get item :call_id) "call-latest")))
                   fallback-input))
                 (diagnostics
                  (plist-get (e-backend-request-metadata recovery-request)
                             :diagnostics)))
            ;; The initial continuation is exactly the matching output delta.
            (should (equal (plist-get incremental :previous_response_id)
                           "resp-latest"))
            (should (equal
                     (mapcar (lambda (item) (plist-get item :type))
                             (plist-get incremental :input))
                     '("function_call_output")))
            ;; The unavailable id is absent from the one complete canonical
            ;; retry, which carries every protocol item needed for replay.
            (should-not (plist-member fallback :previous_response_id))
            (should call)
            (should replay)
            (should result)
            (should (equal (plist-get replay :encrypted_content)
                           "RAW-REPLAY"))
            (should (equal (plist-get result :output) "RAW-RESULT"))
            (should (= (length sends) 3))
            (should-not failure)
            ;; Keep the one-retry assertion tied to the request that was
            ;; actually started; the separate request above must not affect
            ;; the completed recovery count.
            (should (= done-count 2))
            (should (eq (plist-get diagnostics :websocket-request-mode)
                        'full-retry))))))))

(ert-deftest e-openai-test-websocket-idle-timeout-settles-error ()
  "Responses WebSocket idle timeout settles stalled requests as errors."
  (let* ((e-openai-websocket-idle-timeout-seconds 0.01)
         (process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :requires-openai-auth nil)))
         error done-status (close-count 0))
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (&rest _args) 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (&rest _args) t))
              ((symbol-function 'websocket-close)
               (lambda (&rest _args)
                 (cl-incf close-count)
                 t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start backend
                         :messages '((:role user :content "hello"))
                         :options '(:model "gpt-test")
                         :on-item #'ignore
                         :on-done (lambda (status)
                                    (setq done-status status))
                         :on-error (lambda (err) (setq error err)))
        (should (e-openai-test--wait-until (lambda () error) 0.2))
        (should-not done-status)
        (should (eq (car error) 'e-openai-request-timeout))
        (should (= close-count 1))))))

(ert-deftest e-openai-test-websocket-error-diagnostics-are-bounded ()
  "Responses WebSocket library errors do not raw-print unbounded details."
  (let* ((e-openai-diagnostic-print-length 4)
         (e-openai-diagnostic-print-level 3)
         (e-openai-diagnostic-string-max-bytes 48)
         (e-openai-diagnostic-result-max-bytes 220)
         (process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :requires-openai-auth nil)))
         on-error error done-status (close-count 0))
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-error (plist-get args :on-error))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket _text)
                 (funcall on-error
                          websocket
                          :payload (make-list 40 (make-string 100 ?w)))))
              ((symbol-function 'websocket-close)
               (lambda (&rest _args)
                 (cl-incf close-count)
                 t)))
      (e-backend-start (e-openai-backend-create :provider 'openai-websocket)
                       :messages '((:role user :content "hello"))
                       :options '(:model "gpt-test")
                       :on-item #'ignore
                       :on-done (lambda (status)
                                  (setq done-status status))
                       :on-error (lambda (err) (setq error err)))
      (should (e-openai-test--wait-until (lambda () error) 0.2))
      (should-not done-status)
      (should (eq (car error) 'error))
      (should (string-match-p "Responses WebSocket error" (cadr error)))
      (should (string-match-p "OpenAI diagnostic" (cadr error)))
      (should-not (string-match-p (make-string 80 ?w) (cadr error)))
      (should (< (string-bytes (cadr error)) 420))
      (should (= close-count 1)))))

(ert-deftest e-openai-test-websocket-cancel-closes-request ()
  "Responses WebSocket cancellation runs its cleanup path."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :requires-openai-auth nil)))
         (close-count 0)
         request)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (&rest _args) 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (&rest _args) t))
              ((symbol-function 'websocket-close)
               (lambda (&rest _args)
                 (cl-incf close-count)
                 t)))
      (setq request
            (e-backend-start (e-openai-backend-create :provider 'openai-websocket)
                             :messages '((:role user :content "hello"))
                             :options '(:model "gpt-test")
                             :on-item (lambda (_item) nil)
                             :on-done (lambda (_status) nil)
                             :on-error (lambda (_err) nil)))
      (should (e-backend-cancel-request request))
      (should (= close-count 1)))))

(ert-deftest e-openai-test-websocket-store-false-without-anchor-full-replays ()
  "Direct unstored WebSocket calls replay when no prior anchor is supplied."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :response-store :json-false
             :continuation t
             :requires-openai-auth nil)))
         sends on-message done-count seen)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket text)
                 (let* ((payload (json-parse-string text
                                                    :object-type 'plist
                                                    :array-type 'list
                                                    :null-object :json-null
                                                    :false-object :json-false))
                        (index (length sends)))
                   (push payload sends)
                   (funcall on-message
                            websocket
                            (json-encode
                             `(:type "response.output_text.done"
                               :text ,(if (= index 0) "answer one" "answer two"))))
                   (funcall on-message
                            websocket
                            (json-encode
                             `(:type "response.completed"
                               :response
                               (:id ,(if (= index 0) "resp-local-1" "resp-local-2")
                                :status "completed")))))))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start backend
                         :messages '((:role user :content "one"))
                         :options '(:model "gpt-test")
                         :on-item (lambda (item) (push item seen))
                         :on-done (lambda (_status)
                                    (setq done-count (1+ (or done-count 0))))
                         :on-error #'signal)
        (should (e-openai-test--wait-until
                 (lambda () (= (or done-count 0) 1))
                 0.2))
        (e-backend-start backend
                         :messages '((:role user :content "one")
                                     (:role assistant
                                      :content "answer one"
                                      :metadata
                                      (:provider-replay-items
                                       ((:type provider-replay-item
                                         :provider-id openai
                                         :item
                                         (:type "reasoning"
                                          :id "rs-websocket"
                                          :encrypted_content "ciphertext"
                                          :summary
                                          (:type "summary_text"
                                           :text "diagnostic"))))))
                                     (:role user :content "two"))
                         :options '(:model "gpt-test")
                         :on-item (lambda (item) (push item seen))
                         :on-done (lambda (_status)
                                    (setq done-count (1+ (or done-count 0))))
                         :on-error #'signal)
        (should (e-openai-test--wait-until
                 (lambda () (= (or done-count 0) 2))
                 0.2))
        (let* ((first (cadr sends))
               (second (car sends))
               (second-response second))
          (should (eq (plist-get first :store) :json-false))
          (should-not (plist-member first :previous_response_id))
          (should (eq (plist-get second-response :store) :json-false))
          (should-not (plist-member second-response :previous_response_id))
          (should (equal (plist-get second-response :input)
                         '((:type "message"
                            :role "user"
                            :content ((:type "input_text" :text "one")))
                           (:type "reasoning"
                            :id "rs-websocket"
                            :encrypted_content "ciphertext"
                            :summary ((:type "summary_text"
                                       :text "diagnostic")))
                           (:type "message"
                            :role "assistant"
                            :content ((:type "output_text" :text "answer one")))
                           (:type "message"
                            :role "user"
                            :content ((:type "input_text" :text "two"))))))
          ;; The replayed reasoning summary is always an input array, even
          ;; when the provider returned one object in its output item.
          (let ((reasoning (nth 1 (plist-get second-response :input))))
            (should (plist-member reasoning :summary))
            (should (equal (plist-get reasoning :summary)
                           '((:type "summary_text" :text "diagnostic"))))))
        (should (seq-some (lambda (item)
                            (eq (plist-get item :type)
                                'provider-anchor-candidate))
                          seen))))))

(provide 'e-openai-websocket-composition-test)

;;; e-openai-websocket-composition-test.el ends here

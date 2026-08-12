;;; e-e2e-config-eng-ai-gateway.el --- E2E config: Engineering AI gateway -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Context-free live E2E configuration matching the user's Doom
;; `eng-ai-gateway-gpt' OpenAI Responses profile.  Authentication comes only
;; from ENG_AI_MODEL_GW_KEY in the batch process environment.

;;; Code:

(require 'e-default-harnesses)
(require 'e-openai)

(setq e-openai-default-provider 'eng-ai-gateway-gpt
      e-openai-default-model "gpt-5.6-sol")

(setf (alist-get 'eng-ai-gateway-gpt e-openai-model-providers)
      '( :name "Engineering AI Model Gateway / GPT (Responses)"
         :base-url "https://eng-ai-model-gateway.sfproxy.devx-preprod.aws-esvc1-useast2.aws.sfdc.cl"
         :wire-api responses
         :responses-transport http
         :response-store :json-false
         :continuation nil
         :responses-context-layout developer-input
         :prompt-cache-breakpoint-mode explicit
         :include-encrypted-reasoning t
         :requires-openai-auth nil
         :env-key "ENG_AI_MODEL_GW_KEY"
         :default-model "gpt-5.6-sol"))

(cl-defun e-e2e-config--eng-ai-chat-harness-create
    (&key provider sessions layer-ids directory)
  "Create a context-free Engineering AI gateway harness for live E2E.
LAYER-IDS and DIRECTORY are ignored so tests cannot send project context."
  (ignore layer-ids directory)
  (let ((harness
         (e-openai-create-harness
          :provider (or provider e-openai-default-provider)
          :model e-openai-default-model
          :sessions (or sessions (e-default-session-store)))))
    (setf (e-harness-default-options harness)
          (append (e-harness-default-options harness)
                  '(:prompt-cache-default t)))
    harness))

(setq e-default-harness-specs
      '((:id :chat-default
         :name "Default Chat"
         :kind chat
         :default t
         :factory e-e2e-config--eng-ai-chat-harness-create
         :sync e-default-chat-harness-sync)))

(provide 'e-e2e-config-eng-ai-gateway)

;;; e-e2e-config-eng-ai-gateway.el ends here

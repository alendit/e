;;; e-e2e-config-anthropic.el --- E2E backend config: Anthropic gateway -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Sample E_E2E_CONFIG file. Selects the exact gateway Opus 5.5 profile for the
;; Anthropic composed E2E scenario while keeping the existing Opus 4.8 and
;; Sonnet 5 profiles selectable. The live suite loads this file and exercises
;; the configured `:chat-default' harness.
;;
;; Requires ENG_AI_MODEL_GW_KEY in the environment.  Point a run at it with:
;;   E_E2E=1 E_E2E_CONFIG=e2e/e-e2e-config-anthropic.el \
;;     eldev test -f e2e/e-live-e2e-test.el

;;; Code:

(require 'e-anthropic)
(require 'e-default-harnesses)

(setq e-anthropic-default-model "claude-opus-5-5"
      e-anthropic-default-provider 'eng-ai-gateway-opus-5-5
      e-anthropic-model-providers
      '((eng-ai-gateway-opus
         :name "Engineering AI Model Gateway / Opus (Messages)"
         :base-url "https://eng-ai-model-gateway.sfproxy.devx-preprod.aws-esvc1-useast2.aws.sfdc.cl/v1"
         :auth bearer
         :auth-header authorization
         :env-key "ENG_AI_MODEL_GW_KEY"
         :model-prefix ""
         :default-model "claude-opus-4-8")
        (eng-ai-gateway-opus-5-5
         :name "Engineering AI Model Gateway / Opus 5.5 (Messages)"
         :base-url "https://eng-ai-model-gateway.sfproxy.devx-preprod.aws-esvc1-useast2.aws.sfdc.cl/v1"
         :auth bearer
         :auth-header authorization
         :env-key "ENG_AI_MODEL_GW_KEY"
         :model-prefix ""
         :default-model "claude-opus-5-5")
        (eng-ai-gateway-sonnet
         :name "Engineering AI Model Gateway / Sonnet 5 (Messages)"
         :base-url "https://eng-ai-model-gateway.sfproxy.devx-preprod.aws-esvc1-useast2.aws.sfdc.cl/v1"
         :auth bearer
         :auth-header authorization
         :env-key "ENG_AI_MODEL_GW_KEY"
         :model-prefix ""
         :default-model "claude-sonnet-5")
        (eng-ai-gateway-sonnet-5-5
         :name "Engineering AI Model Gateway / Sonnet 5.5 (Messages)"
         :base-url "https://eng-ai-model-gateway.sfproxy.devx-preprod.aws-esvc1-useast2.aws.sfdc.cl/v1"
         :auth bearer
         :auth-header authorization
         :env-key "ENG_AI_MODEL_GW_KEY"
         :model-prefix ""
         :default-model "claude-sonnet-5-5")))

(cl-defun e-e2e-config--chat-harness-create (&key provider sessions layer-ids directory)
  "Create the default chat harness on the native Anthropic Messages adapter."
  (require 'e-base)
  (require 'e-emacs-base)
  (require 'e-harness-base)
  (require 'e-harness)
  (require 'e-layers)
  (e-default-layers-register)
  (let ((harness (e-anthropic-create-harness
                  :provider (or provider e-anthropic-default-provider)
                  :sessions (or sessions (e-default-session-store))))
        (root (or directory default-directory)))
    (e-default-chat-sync-harness-layers harness layer-ids root)
    harness))

(setq e-default-harness-specs
      '((:id :chat-default
         :name "Default Chat"
         :kind chat
         :default t
         :factory e-e2e-config--chat-harness-create
         :sync e-default-chat-harness-sync)))

(provide 'e-e2e-config-anthropic)

;;; e-e2e-config-anthropic.el ends here

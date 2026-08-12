;;; e-e2e-config-openai-api.el --- E2E backend config: OpenAI API -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Sample E_E2E_CONFIG for the canonical first-party OpenAI Responses profile.
;; It requires OPENAI_API_KEY, uses GPT-5.6 over WebSocket with store=false,
;; enables explicit prompt-cache breakpoints, and preserves encrypted reasoning
;; items for stateless replay:
;;   E_E2E=1 E_E2E_CONFIG=e2e/e-e2e-config-openai-api.el \
;;     eldev test -f e2e/e-live-e2e-test.el \
;;     e-live-e2e-test-openai-gpt56-explicit-cache-continues

;;; Code:

(require 'e-default-harnesses)
(require 'e-openai)

(setq e-openai-default-provider 'openai
      e-openai-default-model "gpt-5.6")

(cl-defun e-e2e-config--openai-api-chat-harness-create
    (&key provider sessions layer-ids directory)
  "Create an intentionally context-free first-party OpenAI API test harness.
LAYER-IDS and DIRECTORY are ignored so credentialed tests cannot inherit or
send project-derived default-layer context.  Individual tests add synthetic
capabilities explicitly."
  (ignore layer-ids directory)
  (let ((harness (e-openai-create-harness
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
         :factory e-e2e-config--openai-api-chat-harness-create
         :sync e-default-chat-harness-sync)))

(provide 'e-e2e-config-openai-api)

;;; e-e2e-config-openai-api.el ends here

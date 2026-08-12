;;; e-e2e-config-openai-codex.el --- E2E backend config: ChatGPT Codex -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Sample E_E2E_CONFIG file for GPT-5.6 on the Responses WebSocket transport.
;; It uses Codex-managed ChatGPT authentication and the backend-required
;; unstored response mode.  The current ChatGPT endpoint does not accept the
;; OpenAI API's explicit cache fields, so the focused cache probe skips while
;; the rest of the live suite verifies the supported request shape:
;;   E_E2E=1 E_E2E_CONFIG=e2e/e-e2e-config-openai-codex.el \
;;     eldev test -f e2e/e-live-e2e-test.el \
;;     e-live-e2e-test-openai-gpt56-explicit-cache-continues

;;; Code:

(require 'e-default-harnesses)
(require 'e-openai)

(setq e-openai-default-provider 'codex
      e-openai-default-model "gpt-5.6-sol")

(setf (alist-get 'codex e-openai-model-providers)
      `( :name "ChatGPT Codex"
         :base-url ,(concat e-openai-codex-default-base-url "/codex")
         :wire-api responses
         :responses-transport websocket
         :response-store :json-false
         :prompt-cache-breakpoint-mode nil
         :continuation t
         :requires-openai-auth t))

(cl-defun e-e2e-config--openai-chat-harness-create
    (&key provider sessions layer-ids directory)
  "Create an intentionally context-free GPT-5.6 Codex test harness.
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
         :factory e-e2e-config--openai-chat-harness-create
         :sync e-default-chat-harness-sync)))

(provide 'e-e2e-config-openai-codex)

;;; e-e2e-config-openai-codex.el ends here

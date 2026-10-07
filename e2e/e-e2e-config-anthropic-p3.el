;;; e-e2e-config-anthropic-p3.el --- Context-free P3 candidate config -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; P3 candidate route for the configured gateway Opus 5.5 Messages profile.
;; It reuses the profile from e-e2e-config-anthropic.el but deliberately omits
;; default-layer registration and synchronization.  Authentication comes only
;; from ENG_AI_MODEL_GW_KEY in the batch process environment.
;;
;;   E_E2E=1 E_E2E_CONFIG=e2e/e-e2e-config-anthropic-p3.el \
;;     eldev -s test -f e2e/e-cache-efficiency-e2e-test.el \
;;     e-cache-efficiency-e2e-test-p3-live-native-candidate

;;; Code:

(load (expand-file-name "e-e2e-config-anthropic.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(cl-defun e-e2e-config-anthropic-p3-chat-harness-create
    (&key provider sessions layer-ids directory)
  "Create the isolated Messages candidate harness without user layers."
  (ignore layer-ids directory)
  (e-anthropic-create-harness
   :provider (or provider e-anthropic-default-provider)
   :sessions (or sessions (e-default-session-store))))

(setq e-default-harness-specs
      '((:id :chat-default
         :name "Default Chat"
         :kind chat
         :default t
         :factory e-e2e-config-anthropic-p3-chat-harness-create
         :sync e-default-chat-harness-sync)))

(provide 'e-e2e-config-anthropic-p3)

;;; e-e2e-config-anthropic-p3.el ends here

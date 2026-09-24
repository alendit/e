;;; e-e2e-config-openai-codex-luna.el --- E2E ChatGPT Codex Luna model -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Reuse the context-free Codex fixture with GPT-6 Luna for live model checks.

;;; Code:

(load-file
 (expand-file-name "e-e2e-config-openai-codex.el"
                   (file-name-directory (or load-file-name buffer-file-name))))
(setq e-openai-default-model "gpt-6-luna")

(provide 'e-e2e-config-openai-codex-luna)

;;; e-e2e-config-openai-codex-luna.el ends here

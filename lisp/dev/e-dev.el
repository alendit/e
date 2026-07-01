;;; e-dev.el --- Interactive development helpers for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Development-only helpers for interactive package work inside Emacs.

;;; Code:

(require 'e)

(defgroup e-dev nil
  "Interactive development helpers for e."
  :group 'e
  :prefix "e-dev-")

(defconst e-dev--directory
  (file-name-directory
   (file-truename (or load-file-name buffer-file-name default-directory)))
  "Directory containing this development helper file.")

(defcustom e-dev-source-directory
  (expand-file-name "../.." e-dev--directory)
  "Root directory of the local e checkout used for live reloading."
  :type 'directory
  :group 'e-dev)

(defconst e-dev--obsolete-functions
  '(e-resource-handler-create
    e-resource-handler-p
    e-resource-handler-scheme
    e-resource-handler-read
    e-resource-handler-write
    e-resource-handler-edit
    e-resource-handler-list
    e-capability-resource-handlers
    e-capabilities-register-resource-handlers
    e-skill-create
    e-skill-spec-content
    e-skills-register
    e-emacs-tools-register-buffer-read
    e-emacs-tools-register-buffer-edit)
  "Functions removed from the public surface that reload should unbind.")

(defconst e-dev--obsolete-variables
  '(e-startup-capability-hook)
  "Variables removed from the public surface that reload should unbind.")

(defconst e-dev--reevaluated-defaults
  '(e-openai-default-model
    e-openai-default-reasoning-effort
    e-default-layer-specs
    e-debug-display-strategy
    e-default-chat-layer-ids
    e-output-style-registry)
  "Uncustomized options whose changed defaults should apply after reload.")

(defun e-dev--clear-obsolete-functions ()
  "Remove stale obsolete function bindings after a live reload."
  (dolist (symbol e-dev--obsolete-functions)
    (when (fboundp symbol)
      (fmakunbound symbol))))

(defun e-dev--clear-obsolete-variables ()
  "Remove stale obsolete variable bindings after a live reload."
  (dolist (symbol e-dev--obsolete-variables)
    (when (boundp symbol)
      (makunbound symbol))))

(defun e-dev--reevaluate-uncustomized-defaults ()
  "Reapply changed defcustom defaults unless the user customized them."
  (dolist (symbol e-dev--reevaluated-defaults)
    (when (and (boundp symbol)
               (not (get symbol 'customized-value)))
      (custom-reevaluate-setting symbol))))

;;;###autoload
(defun e-dev-reload (&optional directory)
  "Reload e package files from DIRECTORY or `e-dev-source-directory'."
  (interactive)
  (let* ((root (file-name-as-directory
                (expand-file-name (or directory e-dev-source-directory))))
         (files '("lisp/core/e-events.el"
                  "lisp/core/e-session.el"
                  "lisp/core/e-compaction.el"
                  "lisp/core/e-context.el"
                  "lisp/core/e-context-budget.el"
                  "lisp/core/e-tools.el"
                  "lisp/core/e-operations.el"
                  "lisp/core/e-resource-patterns.el"
                  "lisp/core/e-resources.el"
                  "lisp/core/e-store.el"
                  "lisp/core/e-hooks.el"
                  "lisp/core/e-capability-config.el"
                  "lisp/core/e-capabilities.el"
                  "lisp/core/e-prompts.el"
                  "lisp/core/e-skills.el"
                  "lisp/core/e-agent-shell.el"
                  "lisp/core/e-agent-shell-work.el"
                  "lisp/core/e-mcp.el"
                  "lisp/core/e-startup.el"
                  "lisp/layers/e-layers.el"
                  "lisp/core/e-backend.el"
                  "lisp/core/e-loop.el"
                  "lisp/core/e-harness.el"
                  "lisp/core/e-actions.el"
                  "lisp/layers/e-action-resources.el"
                  "lisp/core/e-harness-registry.el"
                  "lisp/core/e-harness-instances.el"
                  "lisp/core/e-core.el"
                  "lisp/layers/e-layer-selection.el"
                  "lisp/layers/e-context-inspection.el"
                  "lisp/layers/e-runtime-context.el"
                  "lisp/layers/e-layer.el"
                  "lisp/layers/e-dev-layer.el"
                  "lisp/layers/agents/e-agents-std-context.el"
                  "lisp/layers/agents/e-agent-shell-fleet.el"
                  "lisp/layers/base/e-base-tools.el"
                  "lisp/layers/base/e-file-capabilities.el"
                  "lisp/layers/base/e-output-style.el"
                  "lisp/layers/base/e-base.el"
                  "lisp/core/e-raw-results.el"
                  "lisp/layers/harness/e-session-tmp-resources.el"
                  "lisp/layers/harness/e-raw-result-cleanup.el"
                  "lisp/layers/harness/e-tool-output-truncation.el"
                  "lisp/layers/harness/e-harness-base.el"
                  "lisp/layers/emacs/e-elisp-job.el"
                  "lisp/layers/emacs/e-emacs-tools.el"
                  "lisp/layers/emacs/e-emacs-capabilities.el"
                  "lisp/layers/emacs/e-emacs-base.el"
                  "lisp/layers/evidence/e-evidence-tools.el"
                  "lisp/layers/web/e-web-tools.el"
                  "lisp/layers/web/e-web-capabilities.el"
                  "lisp/layers/web/e-web.el"
                  "lisp/layers/org-canvas/e-org-canvas-capabilities.el"
                  "lisp/layers/project-local/e-project-local.el"
                  "lisp/layers/chat/e-chat-session.el"
                  "lisp/adapters/anthropic/e-anthropic.el"
                  "lisp/adapters/openai/e-openai.el"
                  "lisp/defaults/e-default-layers.el"
                  "lisp/defaults/e-default-harnesses.el"
                  "lisp/shells/e-shells.el"
                  "lisp/shells/e-layers-shell.el"
                  "lisp/shells/e-context-status.el"
                  "lisp/shells/e-workspaces.el"
                  "lisp/shells/e-picker.el"
                  "lisp/shells/chat/e-chat.el"
                  "lisp/shells/chat/e-chat-starter.el"
                  "lisp/shells/e-canvas.el"
                  "lisp/shells/e-org-canvas.el"
                  "lisp/shells/e-debug.el"
                  "lisp/dev/e-dev-profile.el"
                  "lisp/dev/e-dev-perf.el"
                  "lisp/dev/e-dev.el")))
    (dolist (file files)
      (load (expand-file-name file root) nil 'nomessage))
    (e-dev--clear-obsolete-functions)
    (e-dev--clear-obsolete-variables)
    (e-dev--reevaluate-uncustomized-defaults)
    (when (fboundp 'e-project-local-reset-loaded-files)
      ;; Project-local factory files are loaded once per session and skipped
      ;; on later opens; forget them so this reload picks up edits on next open.
      (e-project-local-reset-loaded-files))
    (load (expand-file-name "e.el" root) nil 'nomessage)
    (when (fboundp 'e-startup-run)
      (e-startup-run))
    (message "Reloaded e from %s"
             (abbreviate-file-name root))
    root))

(provide 'e-dev)

;;; e-dev.el ends here

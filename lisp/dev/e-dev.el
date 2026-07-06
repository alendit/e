;;; e-dev.el --- Interactive development helpers for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Development-only helpers for interactive package work inside Emacs.

;;; Code:

(require 'e)
(require 'seq)

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
    e-emacs-tools-register-buffer-edit
    e-backend-stream
    e-loop-run-turn
    e-harness-compact-session
    e-harness-follow-up
    e-harness-prompt
    e-harness-wait
    e-tools-execute)
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

(defvar e-dev--reload-required-entries nil
  "Pending explicit reload requests for the running Emacs.")

(defconst e-dev--bytecode-scan-directories
  '("lisp" "test" "e2e")
  "Checkout-local directories scanned for stale e bytecode.")

(defun e-dev--bytecode-source-file (bytecode-file)
  "Return the source file corresponding to BYTECODE-FILE."
  (concat (file-name-sans-extension bytecode-file) ".el"))

(defun e-dev--stale-bytecode-file-p (bytecode-file)
  "Return non-nil when BYTECODE-FILE is older than its source file."
  (let ((source (e-dev--bytecode-source-file bytecode-file)))
    (and (file-exists-p source)
         (file-newer-than-file-p source bytecode-file))))

(defun e-dev--bytecode-candidates (root)
  "Return checkout-local bytecode candidates under ROOT."
  (let ((candidates nil)
        (entrypoint (expand-file-name "e.elc" root)))
    (when (file-exists-p entrypoint)
      (push entrypoint candidates))
    (dolist (directory e-dev--bytecode-scan-directories)
      (let ((path (expand-file-name directory root)))
        (when (file-directory-p path)
          (setq candidates
                (append (directory-files-recursively path "\\.elc\\'")
                        candidates)))))
    (sort candidates #'string<)))

;;;###autoload
(defun e-dev-stale-bytecode-files (&optional directory)
  "Return stale byte-compiled e files under DIRECTORY.
DIRECTORY defaults to `e-dev-source-directory'."
  (let ((root (file-name-as-directory
               (expand-file-name (or directory e-dev-source-directory)))))
    (seq-filter #'e-dev--stale-bytecode-file-p
                (e-dev--bytecode-candidates root))))

;;;###autoload
(defun e-dev-clean-stale-bytecode (&optional directory)
  "Delete stale byte-compiled e files under DIRECTORY.
DIRECTORY defaults to `e-dev-source-directory'.  Only `.elc' files whose
corresponding `.el' source is newer are removed."
  (interactive)
  (let ((files (e-dev-stale-bytecode-files directory)))
    (dolist (file files)
      (delete-file file))
    (when (called-interactively-p 'interactive)
      (message "Deleted %d stale e bytecode file%s"
               (length files)
               (if (= (length files) 1) "" "s")))
    files))

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

(defun e-dev--normalize-file-list (files)
  "Return FILES as a list of strings."
  (cond
   ((null files) nil)
   ((stringp files) (list files))
   ((vectorp files) (e-dev--normalize-file-list (append files nil)))
   ((listp files)
    (delq nil
          (mapcar (lambda (file)
                    (cond
                     ((stringp file) file)
                     ((symbolp file) (symbol-name file))
                     (t nil)))
                  files)))
   (t nil)))

(defun e-dev--reload-required-entry (reason files scope)
  "Create a pending reload entry from REASON FILES and SCOPE."
  (list :id (format "reload-required-%d" (round (* (float-time) 1000)))
        :reason (or reason "e source changed")
        :files (e-dev--normalize-file-list files)
        :scope (or scope 'full)
        :created-at (float-time)))

;;;###autoload
(defun e-dev-mark-reload-required (&optional reason files scope)
  "Record that the running Emacs needs an explicit reload when idle.
This is intentionally only a notification path: it does not load source,
compile files, run startup hooks, or interrupt active work."
  (interactive
   (list (read-string "Reload reason: " nil nil "e source changed")
         nil
         'full))
  (let ((entry (e-dev--reload-required-entry reason files scope)))
    (push entry e-dev--reload-required-entries)
    (message "e reload required: %s; run M-x e-dev-reload when idle"
             (plist-get entry :reason))
    (e-dev-reload-required-status)))

;;;###autoload
(defun e-dev-reload-required-status ()
  "Return pending explicit reload status for the running Emacs."
  (interactive)
  (let ((status (list :required (not (null e-dev--reload-required-entries))
                      :count (length e-dev--reload-required-entries)
                      :entries (nreverse
                                (copy-sequence
                                 e-dev--reload-required-entries)))))
    (when (called-interactively-p 'interactive)
      (if (plist-get status :required)
          (message "e reload required (%d pending)"
                   (plist-get status :count))
        (message "No e reload is pending")))
    status))

(defun e-dev-clear-reload-required ()
  "Clear pending explicit reload requests."
  (interactive)
  (setq e-dev--reload-required-entries nil)
  (when (called-interactively-p 'interactive)
    (message "Cleared pending e reload requests"))
  (e-dev-reload-required-status))

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
                  "lisp/layers/base/e-chat-output-mode.el"
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
    (e-dev-clean-stale-bytecode root)
    (let ((load-prefer-newer t))
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
        (e-startup-run)))
    (e-dev-clear-reload-required)
    (message "Reloaded e from %s"
             (abbreviate-file-name root))
    root))

(provide 'e-dev)

;;; e-dev.el ends here

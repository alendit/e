;;; e-default-layers.el --- Built-in known layer registrations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Registers built-in layer ids lazily.  Concrete layer modules are required
;; only when their layer is created.

;;; Code:

(require 'e-layers)
(require 'e-startup)

(defcustom e-default-layer-specs
  '((:id e
     :name "e"
     :summary "Runtime self-management commands."
     :feature e-layer
     :factory e-core-layer-create)
    (:id e-dev
     :name "e Dev"
     :summary "Development context inspection tools."
     :feature e-dev-layer
     :factory e-dev-layer-create)
    (:id agents-std-context
     :name "Agents Std Context"
     :summary "Standard AGENTS.md and filesystem skill context."
     :feature e-agents-std-context
     :factory e-agents-std-context-layer-create)
    (:id harness-base
     :name "Harness Base"
     :summary "Harness-owned support resources and tool lifecycle guards."
     :feature e-harness-base
     :factory e-harness-base-layer-create)
    (:id os-base
     :name "OS Base"
     :summary "Workspace file and shell tools."
     :feature e-base
     :factory e-base-layer-create)
    (:id emacs-base
     :name "Emacs Base"
     :summary "Live Emacs buffer awareness and editing tools."
     :feature e-emacs-base
     :factory e-emacs-base-layer-create)
    (:id web
     :name "Web"
     :summary "Web search, passive fetch, and browser tools."
     :feature e-web
     :factory e-web-layer-create)
    (:id text-editing
     :name "Text Editing"
     :summary "Progressive guidance for text editing workflows."
     :feature e-text-editing
     :factory e-text-editing-layer-create)
    (:id org-canvas
     :name "Org Canvas"
     :summary "Org document canvas context and visibility tools."
     ;; Load the shell feature, not just `e-org-canvas-capabilities': the shell
     ;; requires capabilities transitively and additionally defines session
     ;; marking (`e-org-canvas--mark-session') that layers depending on
     ;; org-canvas rely on.
     :feature e-org-canvas
     :factory e-org-canvas-layer-create)
    (:id project-local
     :name "Project Local"
     :summary "Capabilities and shells a repository ships under .e/layers/."
     :feature e-project-local
     :factory e-project-local-layer-create))
  "Built-in layer specs registered during startup."
  :type '(repeat sexp)
  :group 'e)

(defun e-default-layers-register (&optional specs)
  "Register built-in known layer SPECS.
When SPECS is nil, register `e-default-layer-specs'."
  (dolist (spec (or specs e-default-layer-specs))
    (e-layer-register
     (e-layer-spec-create
      :id (plist-get spec :id)
      :name (plist-get spec :name)
      :summary (plist-get spec :summary)
      :feature (plist-get spec :feature)
      :factory (plist-get spec :factory)
      :metadata (plist-get spec :metadata))))
  (or specs e-default-layer-specs))

(defun e-default-layers-startup ()
  "Register built-in known layer specs for package startup."
  (e-default-layers-register))

(add-hook 'e-startup-layer-hook #'e-default-layers-startup)

(provide 'e-default-layers)

;;; e-default-layers.el ends here

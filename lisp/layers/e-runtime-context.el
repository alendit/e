;;; e-runtime-context.el --- Model-facing context for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Compact, project-independent context that explains the e runtime to agents.

;;; Code:

(require 'e-capabilities)
(require 'e-store)

(defconst e-runtime-context-instructions
  "e is an Emacs-hosted agent runtime. It is organized around harnesses, sessions, capabilities, layers, tools, resources, hooks, and context providers. Read e://e/refs/runtime.md and e://e/refs/architecture.md when you need to understand how e works."
  "Compact model-facing instructions for the e runtime.")

(defconst e-runtime-context--reference-resources
  '(("refs/runtime.md"
     "e runtime overview"
     "# e runtime overview

e is an Emacs-hosted agent runtime. A harness owns agent lifecycle, sessions, model routing, tool execution, resources, hooks, and context assembly. Presentation shells own buffers, commands, keymaps, rendering, and user interaction.

Capabilities are semantic behavior bundles. They can contribute model instructions, context providers, tools, read-only or mutable resources, lifecycle hooks, and shell-facing actions.

Layers are presets over capabilities. They package useful capability sets for a session without owning durable behavior themselves.

Tools perform model-requested operations. Resources expose addressable content such as `file://`, `buffer://`, `tmp://`, and `e://` references. Context providers add backend-neutral messages before the transcript for each turn.")
    ("refs/architecture.md"
     "e architecture overview"
     "# e architecture overview

The core architecture separates harness, capabilities, backend adapters, and presentation shells.

- The harness owns runtime policy: sessions, active layers, tools, resources, context construction, turn execution, events, and persistence.
- Capabilities own behavior contracts. A capability should have one clear responsibility and expose narrow instructions, tools, resources, hooks, context providers, or actions.
- Layers group capabilities into reusable presets such as `e`, `harness-base`, `os-base`, `emacs-base`, `agents-std-context`, and optional feature layers.
- Backend adapters translate backend-neutral context, tools, and options into provider-specific request shapes.
- Presentation shells own Emacs UI mechanics such as chat buffers, canvas buffers, commands, keymaps, and rendering.

Keep core policy isolated from side effects. Put provider, filesystem, browser, and Emacs UI side effects in adapters, tools, capabilities, or shells that own those concrete concerns."))
  "Reference resources exposed under e://e/refs/.")

(defun e-runtime-context--register-reference-resources (store _capability)
  "Register e runtime reference resources in STORE."
  (dolist (resource e-runtime-context--reference-resources)
    (pcase-let ((`(,path ,description ,content) resource))
      (e-store-register
       store
       'e
       path
       :description description
       :content content))))

(defun e-runtime-context-capability-create ()
  "Create the project-independent e runtime context capability."
  (e-capability-create
   :id 'e-runtime-context
   :name "e Runtime Context"
   :instruction-priority 210
   :instructions e-runtime-context-instructions
   :resources (list #'e-runtime-context--register-reference-resources)))

(provide 'e-runtime-context)

;;; e-runtime-context.el ends here

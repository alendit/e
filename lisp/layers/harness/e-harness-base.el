;;; e-harness-base.el --- Harness support layer for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Built-in harness support layer.  This layer contributes resources and
;; lifecycle hooks owned by the harness rather than by OS or editor tool sets.

;;; Code:

(require 'e-capabilities)
(require 'e-layers)
(require 'e-raw-results)
(require 'e-session-tmp-resources)
(require 'e-session-resources)
(require 'e-raw-result-cleanup)
(require 'e-tool-output-truncation)
(require 'e-await-tool)
(require 'e-store)

(defconst e-harness-base-instructions
  "Communicate reasoning explicitly and concretely, without unnecessary detail. Surface concise reasoning when it changes what the user can understand about the turn: a distinct phase begins, new evidence narrows the work, a decision or tradeoff is made, a blocker appears, the approach changes, or a non-obvious next action is about to happen. Do not send an update for every command or tool call, repeat the same reason for similar commands, restate visible plans, or narrate obvious continuation."
  "Base model-facing instructions contributed by the harness-base layer.")

(defun e-harness-base-context-capability-create ()
  "Create the harness-base context guidance capability."
  (e-capability-create
   :id 'harness-base-context
   :name "Harness Base Context"
   :instruction-priority 240
   :instructions e-harness-base-instructions))

(defconst e-await-reference
  (string-join
   '("# Awaiting async work"
     ""
     "`await' is one model-facing tool that waits, event-driven, until referenced"
     "work settles or a generous timeout expires. Use it instead of polling status"
     "across turns or sleeping: it holds your turn open while the wait runs and never"
     "freezes Emacs."
     ""
     "## References"
     ""
     "A reference is `SCHEME:LOCAL-ID'. Registered schemes name subsystems whose"
     "work is awaitable; a subsystem registers its scheme when its layer is active."
     "The common scheme is `subagent:SUBAGENT-ID'. An unknown scheme or id is"
     "reported per-reference, not a whole-call failure."
     ""
     "## Modes"
     ""
     "- `all' (default): settle when every resolvable reference is terminal."
     "- `any': settle at the first terminal reference (a fan-out race)."
     ""
     "## Result"
     ""
     "On completion `:settled' is t and `:results' gives each reference's terminal"
     "`:state', `:summary', and `:outputs'. On timeout `:settled' is nil, `:reason'"
     "is `timed-out', and the still-pending references are listed so you can await"
     "again or move on. The report never inlines a transcript; pull detail from the"
     "target subsystem on demand (e.g. a subagent's `session://')."
     ""
     "## Pattern"
     ""
     "Fan out, then fan in: start many units of work (e.g. spawn a subagent per"
     "file), then await them all in one call:"
     ""
     "    (await :refs [\"subagent:sub_000003\" \"subagent:sub_000004\"] :mode \"all\" :timeout 180)")
   "\n")
  "Reference exposed as e://await/refs/await.md.")

(defun e-await--register-reference-resources (store capability)
  "Register the await reference resource for CAPABILITY in STORE."
  (e-store-register
   store
   (e-capability-id capability)
   "refs/await.md"
   :description "How to wait on async work with the await tool."
   :content e-await-reference))

(defun e-await-capability-create ()
  "Create the capability contributing the model-facing await tool.
Await is generic over `e-work' handles, so it lives in the base layer every
harness carries rather than in any one subsystem."
  (e-capability-create
   :id 'await
   :name "Await"
   :instruction-priority 238
   :instructions
   (concat
    "To wait for async work (a subagent, a task-queue task, an elisp-job), use "
    "the await tool rather than polling status across turns or sleeping. "
    "Reference work as SCHEME:LOCAL-ID, e.g. subagent:sub_000003. Read "
    "e://await/refs/await.md for modes, the report shape, and the fan-out/fan-in "
    "pattern.")
   :tools (list #'e-await-tool-register)
   :resources (list #'e-await--register-reference-resources)))

(defun e-harness-base-layer-create ()
  "Create the harness-base support layer."
  (e-layer-create
   :id 'harness-base
   :name "Harness Base"
   :capabilities (list (e-harness-base-context-capability-create)
                       (e-await-capability-create)
                       (e-raw-results-capability-create)
                       (e-session-tmp-capability-create)
                       (e-session-resources-capability-create)
                       (e-tool-output-truncation-capability-create))))

(provide 'e-harness-base)

;;; e-harness-base.el ends here

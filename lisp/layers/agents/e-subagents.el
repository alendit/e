;;; e-subagents.el --- Subagent types context and catalog for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Subagents are a capability composed over existing primitives: harness
;; instances, the task queue, `e-work', and the read-only session:// scheme.
;; This module owns the discovery surface: a context provider that lists
;; spawnable subagent types by visibility and a read-only e:// catalog of the
;; full type list.  Spawning, lineage, and lifecycle live in later modules.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-context)
(require 'e-harness-instances)
(require 'e-layers)
(require 'e-skills)
(require 'e-store)
(require 'e-subagent-actions)

(defconst e-subagents-instructions
  "Subagents let this session delegate work to child sessions on purpose-built harness types, keeping the child's whole transcript out of this context. Reach them through e-actions-call, never a model-facing tool. Read e://subagents/skills/subagents for the action contract and e://subagents/refs/types.md for the full catalog of spawnable types."
  "Compact model-facing instructions for the subagents capability.")

(defconst e-subagents-skill
  (string-join
   '("# Subagent work actions"
     ""
     "Subagents delegate work to child sessions on purpose-built harness types. A child's whole transcript stays out of this session's context: you see a handle, a status, and a compact result. The e harness does not know about subagents."
     ""
     "## Choosing a type"
     ""
     "Spawnable types are harness instances tagged as subagents. The always-visible ones are listed in your context; read `e://subagents/refs/types.md' for the full catalog including hidden types. Route a task to the type whose description fits it."
     ""
     "## Actions"
     ""
     "- `spawn`: input `(:type STRING :prompt STRING :seed-messages ARRAY :label STRING :schedule STRING)`. Creates a fresh child session on the type's harness, seeds it (prompt only by default; `:seed-messages` appends explicit context first), records lineage, and starts a non-blocking run. Returns a subagent record immediately. `:schedule` is `direct` (default) or `queue`."
     "- `list`: returns compact records for the current session's direct children, newest-first."
     "- `status`: input `(:subagent-id STRING)`. Returns one full record."
     "- `read`: input `(:subagent-id STRING :raw BOOLEAN :limit INTEGER)`. Default returns the compact result summary plus structured outputs. With `:raw t`, returns a bounded transcript excerpt (last `:limit` messages, default 20) plus the child's `session://` URI, so you can pull detail on demand without the transcript entering your context."
     "- `steer`: input `(:subagent-id STRING :prompt STRING)`. Steers the child's running turn in place."
     "- `send`: input `(:subagent-id STRING :prompt STRING)`. Queues a follow-up turn to the child."
     "- `interrupt`: input `(:subagent-id STRING)`. Aborts the child's active turn, leaving the record inspectable."
     "- `shutdown`: input `(:subagent-id STRING)`. Interrupts if running and marks the record terminal."
     "- `configure-type`: input `(:type STRING :enable-layers ARRAY :disable-layers ARRAY :skills ARRAY)`. Turns individual capabilities on or off for a spawnable type's shared harness, and optionally restricts its `agents-std-context` skill set to named skills. Because children of a type share one harness, this configures the type, not a single child; call it before spawning."
     "- `report` (child-side): input `(:outputs ARRAY :summary STRING)`. A child calls this to set a structured result that overrides its final message. `outputs` entries are `(:kind :value|:uri :label)`."
     ""
     "## Minimal context by default"
     ""
     "The `tool-user` and `fast-tool-user` types start deliberately lean: OS and Emacs tools only, no MCP servers and no filesystem skills. Use them for complex multi-step tool work you want kept out of your own context (a single tool call does not need a subagent). When a child needs more, call `configure-type` first to enable a layer (for example `web` or an MCP layer) or to allow specific skills, then spawn."
     ""
     "## Context discipline"
     ""
     "A child shares one `tmp://` namespace with this session (same lineage). Tell children to write artifacts under `tmp://` and keep their final message terse, e.g. `Result: tmp://sub_result_1.org -- 3 issues found`. That terse final message is the default result; a child that produces artifacts should call `report` instead. Read a child's full transcript only on demand through its `session://` resource.")
   "\n")
  "Skill body documenting the subagents action contract.")

(defun e-subagents--type-line (instance)
  "Return the one-line context entry for subagent INSTANCE."
  (let ((id (e-harness-instance-id instance))
        (name (e-harness-instance-name instance))
        (description (e-harness-instance-description instance)))
    (if (and (stringp description) (not (string-empty-p description)))
        (format "- %s -- %s -- %s" id name description)
      (format "- %s -- %s" id name))))

(defun e-subagents--context-block ()
  "Return the subagents context block, or nil when no types are visible.
Only spawnable instances with `always' context-visibility appear; `hidden'
types stay out of the default context and are read on demand from
e://subagents/refs/types.md."
  (when-let ((instances (e-harness-instance-list-subagents :visibility 'always)))
    (string-join
     (cons
      "Subagent types available to spawn (id -- name -- when to use). Read e://subagents/refs/types.md for the full catalog."
      (mapcar #'e-subagents--type-line instances))
     "\n")))

(defun e-subagents--context-messages ()
  "Return context messages describing spawnable subagent types."
  (when-let ((block (e-subagents--context-block)))
    (list (list :role 'system :content block))))

(defun e-subagents-types-provider ()
  "Return a context provider listing spawnable subagent types by visibility."
  (e-context-provider-create
   :name 'subagents-types
   :priority 210
   :build (cl-function
           (lambda (&key harness session-id turn-id context-purpose)
             (ignore harness session-id turn-id context-purpose)
             (e-subagents--context-messages)))))

(defun e-subagents--catalog-entry (instance)
  "Return the full-catalog markdown entry for subagent INSTANCE."
  (let ((id (e-harness-instance-id instance))
        (name (e-harness-instance-name instance))
        (kind (e-harness-instance-kind instance))
        (visibility (e-harness-instance-context-visibility instance))
        (description (e-harness-instance-description instance)))
    (string-join
     (list (format "## %s" id)
           (format "- name: %s" name)
           (format "- kind: %s" kind)
           (format "- context-visibility: %s" visibility)
           (format "- when to use: %s"
                   (if (and (stringp description)
                            (not (string-empty-p description)))
                       description
                     "(no description)")))
     "\n")))

(defun e-subagents--catalog-markdown ()
  "Return the full markdown catalog of spawnable subagent types."
  (let ((instances (e-harness-instance-list-subagents)))
    (if instances
        (string-join
         (cons "# Subagent types"
               (mapcar #'e-subagents--catalog-entry instances))
         "\n\n")
      (string-join
       '("# Subagent types"
         "No spawnable subagent types are registered.")
       "\n\n"))))

(defun e-subagents--register-reference-resources (store capability)
  "Register subagents reference resources for CAPABILITY in STORE."
  (e-store-register
   store
   (e-capability-id capability)
   "refs/types.md"
   :description "Full catalog of spawnable subagent types."
   :reader (lambda (_entry _range) (e-subagents--catalog-markdown))))

(cl-defun e-subagents-capability-create (&key registry)
  "Create the subagents capability.
Contributes the discovery surface (types context provider and the read-only
type catalog), the skill-backed action contract, and the spawn/observe/steer
actions over REGISTRY (defaults to the process-wide registry)."
  (e-capability-with-skills-create
   :id 'subagents
   :name "Subagents"
   :instruction-priority 235
   :instructions e-subagents-instructions
   :context-providers (list (e-subagents-types-provider))
   :resources (list #'e-subagents--register-reference-resources)
   :actions (e-subagent-actions-alist registry)
   :skills (list (e-skill-spec-create
                  :name "subagents"
                  :description "Spawn, observe, steer, and shut down child subagent sessions."
                  :content e-subagents-skill))))

(defun e-subagents-layer-create ()
  "Create the subagents layer."
  (e-layer-create
   :id 'subagents
   :name "Subagents"
   :capabilities (list (e-subagents-capability-create))))

(provide 'e-subagents)

;;; e-subagents.el ends here

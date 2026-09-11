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
(require 'seq)
(require 'e-capabilities)
(require 'e-context)
(require 'e-harness-instances)
(require 'e-layers)
(require 'e-skills)
(require 'e-store)
(require 'e-subagent-actions)
(require 'e-board-orchestration-actions)
(require 'e-subagent-registry)
(require 'e-waitable)

(defconst e-subagents-instructions
  "Subagents let this session delegate work to child sessions on purpose-built harness types, keeping the child's whole transcript out of this context. Reach them through e-actions-call, never a model-facing tool. Await timeouts are supervision checkpoints: use progress evidence before waiting again, steering, or explicitly interrupting. Read e://subagents/skills/subagents for the action contract and e://subagents/refs/types.md for the full catalog of spawnable types."
  "Compact model-facing instructions for the subagents capability.")

(defconst e-subagents-child-instructions
  "You are a subagent: a child session spawned to do one task and return a compact result. Write artifacts under tmp:// (shared with the parent) and keep your final message terse. When you produce artifacts, set a structured result with (e-actions-call 'subagents :report '(:summary \"...\" :outputs [(:kind org-link :uri \"tmp://...\" :label \"...\")])). Otherwise your final message is the result."
  "Compact model-facing instructions for the child-facing subagents capability.")

(defconst e-subagents-skill
  (string-join
   '("# Subagent work actions"
     ""
     "Subagents delegate work to child sessions on purpose-built harness types. A child's whole transcript stays out of this session's context: you see a handle and live execution status. Terminal results and history are Board facts queried from SQLite; the live registry releases the child when it settles. The e harness does not know about subagents."
     ""
     "## Choosing a type"
     ""
     "Spawnable types are harness instances tagged as subagents. The always-visible ones are listed in your context; read `e://subagents/refs/types.md' for the full catalog including hidden types. Route a task to the type whose description fits it."
     ""
     "## Actions"
     ""
     "An action that needs asynchronous SQLite work returns a work: reference. Pass it to the top-level await tool and consume its bounded settlement report; do not treat the reference as the action result or poll the action again."
     ""
     "- `spawn`: input `(:type STRING :prompt STRING :seed-messages ARRAY :label STRING :schedule STRING)`. Creates a fresh child session on the type's harness, seeds it (prompt only by default; `:seed-messages` appends explicit context first), records lineage, and starts a non-blocking run. Returns a subagent record immediately. `:schedule` is `direct` (default) or `queue`."
     "- `list`: returns compact live records for the current session's executing direct children, newest-first."
     "- `list-runs`: returns bounded durable run projections from this session's board."
     "- `run-status`: input `(:run-id STRING)`. Returns one bounded durable run projection with task states, reports, conflicts, deadline evidence, and continuation state."
     "- `status`: input `(:subagent-id STRING)`. Returns one live execution record."
     "- `read`: input `(:subagent-id STRING :raw BOOLEAN :limit INTEGER)`. While the child is live, default returns its compact reported summary plus structured outputs. With `:raw t`, returns a bounded transcript excerpt (last `:limit` messages, default 20) plus the child's `session://` URI, so you can pull detail on demand without the transcript entering your context."
     "- `steer`: input `(:subagent-id STRING :prompt STRING :reason STRING)`. Steers the child's running turn in place. `:reason` is bounded audit data and reaches the child only through `:prompt`."
     "- `send`: input `(:subagent-id STRING :prompt STRING)`. Queues a follow-up turn to the child."
     "- `interrupt`: input `(:subagent-id STRING :reason STRING)`. Explicitly aborts the child's active turn and retires its live record. `:reason` is audit data only."
     "- `shutdown`: input `(:subagent-id STRING :reason STRING)`. Explicitly interrupts a running child and retires its live record. `:reason` is audit data only."
     "- `configure-type`: input `(:type STRING :enable-layers ARRAY :disable-layers ARRAY :layer-config ALIST)`. Turns individual capabilities on or off for a spawnable type's shared harness. `layer-config` maps a capability id to its option plist, the generic way to pass or overwrite a layer's configuration -- e.g. `((agents-std-context :skills-include (\"writing\")))` to allow only the `writing` skill, or `:skills-exclude` to deny a few. Because children of a type share one harness, this configures the type, not a single child; call it before spawning."
     "- `report` (child-side): input `(:outputs ARRAY :summary STRING)`. A child calls this to set a structured result that overrides its final message. `outputs` entries are `(:kind :value|:uri :label)`."
     ""
     "## Minimal context by default"
     ""
     "The `tool-user` and `fast-tool-user` types start deliberately lean: OS and Emacs tools only, no MCP servers and no filesystem skills. Use them for complex multi-step tool work you want kept out of your own context (a single tool call does not need a subagent). When a child needs more, call `configure-type` first to enable a layer (for example `web` or an MCP layer) or to allow specific skills, then spawn."
     ""
     "## Context discipline"
     ""
     "A child shares one `tmp://` namespace with this session (same lineage). Tell children to write artifacts under `tmp://` and keep their final message terse, e.g. `Result: tmp://sub_result_1.org -- 3 issues found`. That terse final message is the default result; a child that produces artifacts should call `report` instead. Read a child's full transcript only on demand through its `session://` resource."
     "Delegate by replacement, not duplication. Give each child a mutually exclusive scope and use its compact result instead of repeating the same source traversal in the parent. Do not spawn overlapping children unless their independently required outputs justify the duplicate reads. If the parent must validate a child finding, inspect only the cited range or artifact."
     ""
     "## Waiting for children"
     ""
     "Do not poll a child's status across turns, and never sleep to wait. Use the `await` tool (a model-facing tool, not an action): reference each child as `subagent:SUBAGENT-ID`, e.g. `(await :refs [\"subagent:sub_000003\" \"subagent:sub_000004\"] :mode \"all\" :timeout 90)`. It blocks your turn -- not Emacs -- until the referenced children settle (`all`, default) or the first settles (`any`), or the timeout expires. Use `any` when useful parent work can consume the first result while other children continue; use `all` only when synthesis genuinely requires every result."
     ""
     "A timeout is a supervision checkpoint, not a terminal error. Compare each pending child's progress sequence and age with the prior checkpoint. Re-await only when progress advanced or a declared long operation remains credible. When progress is unchanged, inspect with `status` or bounded `read :raw t`, then steer once with one concrete next action. If the post-steer checkpoint is still unchanged, explicitly `interrupt` and spawn a fresh, narrower child only when the work is still required. Time alone never authorizes cancellation. This is the fan-in step after a fan-out: spawn one child per non-overlapping unit, then await them in one call.")
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

(defun e-subagents--runtime-label (record now)
  "Return a short runtime label for RECORD at NOW."
  (if-let ((started-at (plist-get record :started-at)))
      (format "%.0fs" (max 0.0 (- now started-at)))
    "not started"))

(defun e-subagents--active-children-context (registry parent-session-id)
  "Return bounded supervision evidence for PARENT-SESSION-ID's live children."
  (let ((records (seq-filter (lambda (record)
                               (memq (plist-get record :status)
                                     '(queued running blocked)))
                             (e-subagent-registry-list registry parent-session-id))))
    (when records
      (let ((now (float-time)))
        (string-join
         (append
          (list "Active child supervision.  Await timeouts are checkpoints: compare progress, inspect unchanged work, steer once, then explicitly interrupt and choose resume or replacement.  Time alone never authorizes cancellation.")
          (mapcar
           (lambda (record)
             (let* ((progress (plist-get record :progress))
                    (last-activity (plist-get record :last-activity-at))
                    (age (if last-activity
                             (format "%.0fs" (max 0.0 (- now last-activity)))
                           "none")))
               (format "- %s: %s; runtime %s; last progress %s ago; progress %s"
                       (plist-get record :subagent-id)
                       (plist-get record :status)
                       (e-subagents--runtime-label record now)
                       age
                       (format "#%s %s"
                               (or (plist-get record :progress-sequence) 0)
                               (or (plist-get progress :summary) "none")))))
           (seq-take records 16)))
         "\n")))))

(defun e-subagents--context-messages (&optional registry session-id)
  "Return context messages describing types and active direct children."
  (let* ((registry (or registry e-subagent-actions-default-registry))
        (blocks (delq nil (list (e-subagents--context-block)
                                (and session-id
                                     (e-subagents--active-children-context
                                      registry session-id))))))
    (when blocks
      (list (list :role 'system :content (string-join blocks "\n\n"))))))

(defun e-subagents-types-provider (&optional registry)
  "Return a context provider listing types and live supervision evidence."
  (let ((registry (or registry e-subagent-actions-default-registry)))
    (e-context-provider-create
     :name 'subagents-types
     :priority 210
     :build (cl-function
             (lambda (&key harness session-id turn-id context-purpose)
               (ignore harness turn-id context-purpose)
               (e-subagents--context-messages registry session-id))))))

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

(cl-defun e-subagents-parent-capability-create (&key registry)
  "Create the parent-facing subagents capability.
Contributes the discovery surface (types context provider and the read-only
type catalog), the skill-backed action contract, and the parent-facing
spawn/observe/steer/configure actions over REGISTRY (defaults to the
process-wide registry).  A session enables this to spawn and manage children."
  (e-capability-with-skills-create
   :id 'subagents
   :name "Subagents"
   :instruction-priority 235
   :instructions e-subagents-instructions
   :context-providers (list (e-subagents-types-provider registry))
   :resources (list #'e-subagents--register-reference-resources)
   :actions (append (e-subagent-actions-parent-alist registry)
                    (e-board-orchestration-actions-parent-alist))
   :skills (list (e-skill-spec-create
                  :name "subagents"
                  :description "Spawn, observe, steer, and shut down child subagent sessions."
                  :content e-subagents-skill))))

(cl-defun e-subagents-child-capability-create (&key registry)
  "Create the child-facing subagents capability.
A spawned child carries this so it can set a structured result for its own
session via the `report' action.  It deliberately omits the spawn surface, the
types context, and the type catalog, so a lean child stays lean.

Its capability id is `subagents' -- the same the parent uses -- so a child's
\='(e-actions-call \='subagents :report ...) resolves against whichever subagents
capability variant its harness carries.  The two variants never coexist on one
harness (parent on the spawning session, child on the spawned session), so the
shared id causes no action or resource collision."
  (e-capability-create
   :id 'subagents
   :name "Subagents (child)"
   :instruction-priority 235
   :instructions e-subagents-child-instructions
   :actions (e-subagent-actions-child-alist registry)))

(defun e-subagents-register-waitable-resolver (&optional registry)
  "Register the `subagent' waitable scheme against REGISTRY.
A reference of the form subagent:SUBAGENT-ID resolves to that subagent's live
`e-work' handle, so the generic `await' tool can wait on a child without knowing
about subagents.  REGISTRY defaults to the process-wide subagent registry."
  (let ((registry (or registry e-subagent-actions-default-registry)))
    (e-waitable-register-resolver
     "subagent"
     (lambda (id) (e-subagent-registry-work-handle registry id)))))

(defun e-subagents-parent-layer-create ()
  "Create the parent-facing subagents layer, enabled on sessions that spawn."
  (e-subagents-register-waitable-resolver)
  (e-layer-create
   :id 'subagents-parent
   :name "Subagents (parent)"
   :requires '(async-control)
   :capabilities (list (e-subagents-parent-capability-create))))

(defun e-subagents-child-layer-create ()
  "Create the child-facing subagents layer added to every spawned child."
  (e-layer-create
   :id 'subagents-child
   :name "Subagents (child)"
   :capabilities (list (e-subagents-child-capability-create))))

(provide 'e-subagents)

;;; e-subagents.el ends here

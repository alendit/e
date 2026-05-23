# Skills over Capabilities Plan

## Implementation Review

Post-implementation review for commit `eeabcf6` lives at [`../../../review.md`](../../../review.md).

## Goal

Refactor skills so they are not a first-level runtime construct. Capabilities remain the primitive unit exposed to the harness. Progressive discovery should be expressed with the mechanisms capabilities already have:

1. initial instructions, which advertise compact affordances and point at details;
2. resource contributions, which register detailed bodies under readable URIs; and
3. the existing resource operation tools, especially `read`, which let the model load details on demand.

A "skill" should become construction-time convenience only: a small builder/spec that appends a generated preamble to a capability's `:instructions` and registers the skill body as a normal `e://` resource. After construction, the harness should see only an ordinary `e-capability` with instructions and resources. It should not know that any resource came from a skill.

## Current State to Change

The current implementation treats skills as a distinct model-facing catalog:

- `lisp/core/e-skills.el` defines `e-skill`, registration, listing, and catalog rendering.
- `e-skills-register` stores skill bodies in the `e://` store with `:kind 'skill` metadata.
- `e-skills-list` and `e-skills-catalog-text` scan the store for skill metadata.
- `e-harness--skill-catalog-context-messages` injects a skill catalog into every context.
- `e-harness-context` special-cases that catalog before normal capability context messages.
- Tests assert that this harness-level catalog appears in backend messages.

This creates an unnecessary first-level "skill" concept in the harness. The desired shape is simpler: a capability can include a preamble saying "read these URIs when relevant", and can register those URIs as resources. That is enough for progressive discovery.

## Target Design

### Capability remains the runtime primitive

The harness should continue to derive behavior from active capabilities only:

- `:instructions` become system messages via `e-capabilities-context-messages`.
- `:resources` populate the capability-scoped `e://` store.
- active resource methods expose `read`/`write`/`edit` tools as today.

No harness code should ask "what skills are active?" or scan store metadata for skills. Skills are not part of the harness contract.

### Skill is a builder/spec, not a runtime category

Keep or replace `e-skills.el` as optional construction sugar. It should provide a way to describe one or more skill-like discoverable instruction bodies, then build an ordinary capability from them.

Suggested public API:

```elisp
(cl-defstruct (e-skill-spec
               (:constructor e-skill-spec-create
                             (&key name description content reader path metadata)))
  name
  description
  content
  reader
  path
  metadata)

(cl-defun e-capability-with-skills-create
    (&key id name instructions skills tools resource-methods resources
          context-providers actions skill-heading)
  "Create an ordinary capability with discoverable skill resources.")
```

Naming can be adjusted, but preserve the architectural rule: the result must be an `e-capability`, not a new runtime entity consumed by the harness.

### Multi-skill convenience

The builder must support more than one skill. Example intended use:

```elisp
(e-capability-with-skills-create
 :id 'code-assistant
 :name "Code Assistant"
 :instructions "Use these capabilities for code-related work."
 :skills
 (list
  (e-skill-spec-create
   :name "code-review"
   :description "Review implementation changes for correctness and maintainability."
   :content "Full code review instructions...")
  (e-skill-spec-create
   :name "debugging"
   :description "Diagnose failing tests or runtime errors."
   :content "Full debugging workflow...")
  (e-skill-spec-create
   :name "refactor"
   :description "Plan and apply safe refactors."
   :content "Full refactoring guidance...")))
```

This should produce an ordinary capability equivalent to:

```elisp
(e-capability-create
 :id 'code-assistant
 :name "Code Assistant"
 :instructions
 "Use these capabilities for code-related work.

Additional guidance is available on demand. Load only what is relevant:
- code-review: Review implementation changes for correctness and maintainability. Read e://code-assistant/skills/code-review
- debugging: Diagnose failing tests or runtime errors. Read e://code-assistant/skills/debugging
- refactor: Plan and apply safe refactors. Read e://code-assistant/skills/refactor"
 :resources
 (list
  (lambda (store capability)
    (e-store-register
     store
     (e-capability-id capability)
     "skills/code-review"
     :description "Review implementation changes for correctness and maintainability."
     :content "Full code review instructions...")
    (e-store-register
     store
     (e-capability-id capability)
     "skills/debugging"
     :description "Diagnose failing tests or runtime errors."
     :content "Full debugging workflow...")
    (e-store-register
     store
     (e-capability-id capability)
     "skills/refactor"
     :description "Plan and apply safe refactors."
     :content "Full refactoring guidance..."))))
```

Existing `:tools`, `:resource-methods`, `:resources`, `:context-providers`, and `:actions` passed to the builder should be preserved. Skill resource registration should be appended to any caller-provided `:resources` without mutating those inputs.

## Detailed Semantics

### Skill spec validation

A skill spec should be a construction-time descriptor with at least:

- `:name`: required string, non-empty, no `/` if it is used in the default path.
- `:description`: required string, compact model-facing summary.
- `:content` or `:reader`: required. `:content` is a string. `:reader` is a function compatible with the store reader pattern.
- `:path`: optional explicit store path. If omitted, default to `skills/<name>`.
- `:metadata`: optional metadata copied onto the store entry for debugging/UI use only.

Do not require or depend on `:kind 'skill` metadata for core behavior. If metadata is retained for compatibility or inspection, it must not be used by the harness.

### URI and preamble generation

For each skill, compute the URI with `e-store-uri` from the capability id and the skill path. The default URI should remain conventional:

```text
e://<capability-id>/skills/<skill-name>
```

Generate one compact instruction preamble containing all skills for the capability. Default text can be:

```text
Additional guidance is available on demand. Load only what is relevant:
- <name>: <description> Read <uri>
```

`skill-heading` may override the default heading. The preamble is appended to any caller-supplied `:instructions`, separated by a blank line. If there are no skills, do not add an empty preamble.

### Resource registration

The builder should add one resource provider function to the capability. That provider should register each skill body into the passed store using `e-store-register` and the active capability id. It should be idempotent in the same sense as other capability providers: deriving a fresh store from the same capability repeatedly produces the same resources. It does not need to support registering the same resource twice into the same store, because `e-store-register` correctly rejects duplicates.

Dynamic readers should continue to work. If a skill has a `:reader`, wrap it so that it receives the skill spec and the optional range, or document the exact calling convention. Keep the calling convention narrow and test it.

### Immutability/idempotence expectations

Architecturally, capabilities should be treated as immutable/idempotent bundles after construction, even though Emacs Lisp structs are technically mutable. The skill builder should not mutate an existing capability. Prefer constructing a new `e-capability` from supplied parts. If a helper for adding skills to an existing capability is introduced, it should return a new capability copy rather than modifying the original.

## Implementation Steps

1. Refactor `lisp/core/e-skills.el` from a runtime catalog module into builder sugar.
   - Replace or supplement `e-skill` with `e-skill-spec`.
   - Add helpers for normalized path, URI generation, preamble generation, and resource provider generation.
   - Add `e-capability-with-skills-create` or equivalent.

2. Remove harness-level skill catalog injection.
   - Delete `e-harness--skill-catalog-context-messages`.
   - Remove its call from `e-harness-context`.
   - Remove `require 'e-skills` from `e-harness.el` unless still needed for another reason.

3. Keep the generic `e://` store behavior.
   - `e-store` remains read-only and capability-scoped.
   - `e-store-resource-method` remains the way the generated skill bodies become readable through the normal `read` tool.
   - Do not add a skill-specific read tool.

4. Update tests.
   - Remove or rewrite tests that expect an automatic skill catalog in context.
   - Add tests that `e-capability-with-skills-create` returns an `e-capability`.
   - Assert that generated instructions include each skill's name, description, and URI, but not the full body.
   - Assert that `e-harness-context` includes that generated preamble through normal capability instructions.
   - Assert that `read` can load each generated skill body from `e://<capability>/skills/<name>`.
   - Assert that multiple skills register in order and all are discoverable through the preamble.
   - Assert that caller-provided resources still register alongside generated skill resources.
   - Assert read-only behavior remains: `e://` skill bodies do not support write/edit.

5. Update docs.
   - Update `docs/architecture.md` public surface language so skills are described as builder sugar over capabilities, not as a harness catalog mechanism.
   - Update any MVP/architecture notes that currently imply skills are a first-level runtime construct.

6. Compatibility decision.
   - Prefer removing obsolete first-level catalog functions from core usage.
   - If keeping old names temporarily, make them thin compatibility wrappers around the new builder helpers and mark them as compatibility-only in docstrings/tests.
   - Do not keep harness scanning behavior for compatibility unless there is a concrete caller that requires it.

## Acceptance Criteria

- The harness has no skill-specific context injection or store scanning.
- A multi-skill builder can create a normal `e-capability` with generated instructions and generated `e://` resources.
- Progressive discovery works through ordinary capability instructions plus the existing `read` tool.
- The initial context contains skill preambles/references but not full skill bodies.
- Full skill bodies can be loaded on demand with `read`.
- Tests cover multi-skill capabilities and ensure existing non-skill capability resource behavior still works.
- Documentation presents skills as convenience construction sugar over capabilities, not as a first-level harness concept.

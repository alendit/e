# Architecture Alignment Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:subagent-driven-development` or `superpowers:executing-plans` to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move `e` from layer-owned behavior bundles toward capability-owned behavior contracts with layers as packaging presets.

**Architecture:** Capabilities become the semantic unit for tools, context providers, prompt fragments, and shell-facing actions. Layers become presets that activate capability sets. The harness remains the stable runtime substrate, while `e-chat` becomes a presentation host over `chat-session` behavior instead of owning chat semantics directly.

**Tech Stack:** Emacs Lisp, ERT, Eldev, existing `lisp/e-*.el` modules, existing JSONL session store.

**Out of scope for this plan:** Permission/audit policy. The architecture document names it as a future concern, but this alignment pass should not implement policy prompts, approval records, or audit gates.

---

## Completed Alignment Summary

- `e-layer` is now a pure preset over capability objects and defaults.
- `base` and `emacs-base` are implemented as layer presets over focused capabilities.
- `e-chat` delegates chat-session semantics through `e-chat-session`; presentation remains responsible for buffers, keymaps, rendering, and user interaction.
- Context assembly collects instructions and providers from active capabilities.
- The session store now records transcript/activity evidence plus branch summaries, compactions, and current branch state, with read-only evidence retrieval tools.
- Abort now cancels queued async turns and active backend request handles when adapters expose a cancellable request.
- The OpenAI backend boundary remains adapter-owned; the current synchronous `url.el` request path exposes an explicit non-cancellable handle until a cancellable adapter exists.

## File Structure

- Create `lisp/e-capabilities.el`: capability data model and helpers for activating tools/context/action contributions.
- Create `test/e-capabilities-test.el`: unit tests for capability construction and contribution collection.
- Modify `lisp/e-layers.el`: make layers package capability ids/objects and optional defaults, while keeping a narrow bridge for current behavior during migration.
- Modify `lisp/e-harness.el`: track active capabilities, activate layer presets by resolving capabilities, and build context from active capabilities.
- Modify `lisp/e-base.el`: make `base` a preset over file capabilities.
- Modify or create `lisp/e-file-capabilities.el`: file inspection, file mutation, and shell/process capabilities.
- Modify `lisp/e-emacs-base.el`: make `emacs` or `emacs-base` a preset over Emacs capabilities.
- Modify or create `lisp/e-emacs-capabilities.el`: Emacs awareness, buffer read, buffer edit/save, elisp eval, and selection context capabilities.
- Create `lisp/e-chat-session.el`: chat-session capability actions.
- Modify `lisp/e-chat.el`: keep rendering/keymaps/composer mechanics; delegate semantic actions to `e-chat-session`.
- Modify `lisp/e-core.el`: require new capability modules.
- Modify `e.el` and `test/e-test.el` only if new public functions must be exposed through `(require 'e)`.
- Add or modify focused tests under `test/` for each slice.

## Phase 1: Capability Contract

### Task 1: Add Capability Data Model

**Files:**
- Create: `lisp/e-capabilities.el`
- Create: `test/e-capabilities-test.el`
- Modify: `lisp/e-core.el`
- Modify: `test/e-test.el`

- [ ] **Step 1: Write tests for the capability shape**

Add tests that prove a capability can carry independent contribution types:

```elisp
(ert-deftest e-capabilities-test-create-capability ()
  (let ((capability
         (e-capability-create
          :id 'buffer-read
          :name "Buffer Read"
          :instructions "Read buffers."
          :tools (list #'ignore)
          :context-providers nil
          :actions '(:read-buffer #'ignore))))
    (should (eq (e-capability-id capability) 'buffer-read))
    (should (equal (e-capability-name capability) "Buffer Read"))
    (should (equal (e-capability-instructions capability) "Read buffers."))
    (should (= (length (e-capability-tools capability)) 1))
    (should (plist-member (e-capability-actions capability) :read-buffer))))
```

- [ ] **Step 2: Implement `e-capabilities.el`**

Define:

```elisp
(cl-defstruct (e-capability (:constructor e-capability-create))
  id
  name
  instructions
  tools
  context-providers
  actions)
```

Add helpers with these contracts:

- `e-capabilities-register-tools`: accepts one capability and one tool registry, then calls each function in `(e-capability-tools capability)` with the registry.
- `e-capabilities-context-messages`: accepts a capability list plus `:harness`, `:session-id`, and `:turn-id`, then returns instruction messages followed by provider-built messages.
- `e-capabilities-action`: accepts one capability and an action keyword, then returns the function stored in `(e-capability-actions capability)`.

Do not add permission/audit fields in this pass.

- [ ] **Step 3: Wire the module**

Require `e-capabilities` from `lisp/e-core.el`. If the API should be public, ensure `(require 'e)` exposes it and add smoke assertions to `test/e-test.el`.

- [ ] **Step 4: Verify**

Run:

```bash
eldev test test/e-capabilities-test.el test/e-test.el
```

Expected: all selected tests pass.

- [ ] **Step 5: Commit**

Commit only the capability contract slice.

## Phase 2: Layers As Presets

### Task 2: Make Layers Resolve Capabilities

**Files:**
- Modify: `lisp/e-layers.el`
- Modify: `lisp/e-harness.el`
- Modify: `test/e-layers-test.el`
- Modify: `test/e-harness-test.el`

- [ ] **Step 1: Add tests for layer-packaged capabilities**

Add coverage for a layer that activates two capabilities and contributes both instruction/context and tools through those capabilities.

The expected behavior:

- activating one layer activates both capability contributions
- context messages come from capabilities
- tool definitions come from capabilities
- layer metadata itself does not need direct tool/context fields

- [ ] **Step 2: Extend `e-layer`**

Change `e-layer` toward:

```elisp
(cl-defstruct (e-layer (:constructor e-layer-create))
  id
  name
  capabilities
  defaults
  instructions
  tools
  context-providers
  skills
  prompts)
```

Keep the existing fields temporarily as a migration bridge so current layers do not break inside this slice.

- [ ] **Step 3: Add active capability state to the harness**

Extend the harness struct to track active capabilities separately from active layers. `e-harness-activate-layer` should:

- append the layer to active layers
- resolve capability objects from `(e-layer-capabilities layer)`
- append them to active capabilities
- register capability tools
- keep registering legacy layer tools while the migration bridge exists

- [ ] **Step 4: Build context from capabilities first**

Update `e-harness-context` so prefix messages come from active capabilities. During the bridge period, also include legacy layer context messages after capability messages only for layers that have not yet been migrated.

- [ ] **Step 5: Verify**

Run:

```bash
eldev test test/e-layers-test.el test/e-harness-test.el
```

Expected: existing layer tests still pass, and new capability-backed layer tests pass.

- [ ] **Step 6: Commit**

Commit the layer-resolution bridge before migrating concrete layers.

## Phase 3: Base Layer Split

### Task 3: Extract File Capabilities

**Files:**
- Create or modify: `lisp/e-file-capabilities.el`
- Modify: `lisp/e-base.el`
- Modify: `lisp/e-core.el`
- Modify: `test/e-base-test.el`
- Modify or add: `test/e-file-capabilities-test.el`

- [ ] **Step 1: Add tests for file capability activation**

Cover these expected capability sets:

- `file-inspection` registers only the read tool
- `file-mutation` registers write and edit tools
- `shell-process` registers bash
- `base` layer activates all three

- [ ] **Step 2: Implement `file-inspection`**

Create `e-file-inspection-capability-create`. It accepts `directory` and returns an `e-capability` whose tool provider only calls `e-base-tools-register-read`.

- [ ] **Step 3: Implement `file-mutation`**

Create `e-file-mutation-capability-create`. It accepts `directory` and returns an `e-capability` whose tool provider registers `write` and `edit` only.

- [ ] **Step 4: Implement `shell-process`**

Create `e-shell-process-capability-create`. It accepts `directory` and returns an `e-capability` whose tool provider registers `bash` only.

- [ ] **Step 5: Convert `base` to a preset**

Update `e-base-layer-create` so it returns a layer with `:capabilities` set to the three file capability objects. Keep the current base instruction text either as:

- a small `base-guidance` capability, or
- a temporary legacy layer instruction until all context assembly is capability-backed.

Prefer `base-guidance` if it keeps the implementation simple.

- [ ] **Step 6: Verify**

Run:

```bash
eldev test test/e-base-test.el test/e-base-tools-test.el test/e-file-capabilities-test.el
```

Expected: base layer still exposes `read`, `write`, `edit`, and `bash`; individual capability tests prove narrower activation.

- [ ] **Step 7: Commit**

Commit the base split separately from Emacs and chat work.

## Phase 4: Emacs Layer Split

### Task 4: Extract Emacs Capabilities

**Files:**
- Create or modify: `lisp/e-emacs-capabilities.el`
- Modify: `lisp/e-emacs-base.el`
- Modify: `lisp/e-core.el`
- Modify: `test/e-emacs-base-test.el`
- Modify or add: `test/e-emacs-capabilities-test.el`

- [ ] **Step 1: Add tests for Emacs capability activation**

Cover these expectations:

- `emacs-awareness` contributes instructions and visible-buffer context
- `buffer-read` registers `list_buffers` and `read_buffer`
- `buffer-edit` registers `write_buffer`, `edit_buffer`, and `save_buffer`
- `elisp-eval` registers `run_elisp`
- `selection-context` exists as a capability placeholder only if no current selection implementation exists
- `emacs` layer activates `emacs-awareness`, `buffer-read`, and optionally `selection-context`
- `emacs-operator` layer activates `emacs` plus `buffer-edit` and `elisp-eval`

- [ ] **Step 2: Split tool registrars if needed**

The old aggregate `e-emacs-tools-register-defaults` path is too coarse; add focused registration helpers:

- `e-emacs-tools-register-buffer-read`: registers `list_buffers` and `read_buffer`.
- `e-emacs-tools-register-buffer-edit`: registers `write_buffer`, `edit_buffer`, and `save_buffer`.
- `e-emacs-tools-register-elisp-eval`: registers `run_elisp`.

Reuse the existing individual tool registration functions instead of duplicating handlers.

- [ ] **Step 3: Implement `emacs-awareness`**

Move current visible-buffer context provider construction behind `e-emacs-awareness-capability-create`. It should carry the current Emacs instruction text and visible-buffer context provider.

- [ ] **Step 4: Implement buffer and elisp capabilities**

Add these capability constructors:

- `e-buffer-read-capability-create`
- `e-buffer-edit-capability-create`
- `e-elisp-eval-capability-create`
- `e-selection-context-capability-create`

`selection-context` can be a no-op capability with a clear test proving it activates without adding tools until selection capture is implemented.

- [ ] **Step 5: Convert `emacs-base`**

Keep `e-emacs-base-layer-create` as a compatibility preset for now, but implement it by composing capabilities. Add `e-emacs-layer-create` and `e-emacs-operator-layer-create` only if the tests need distinct conservative and operator presets.

- [ ] **Step 6: Verify**

Run:

```bash
eldev test test/e-emacs-base-test.el test/e-emacs-tools-test.el test/e-emacs-capabilities-test.el
```

Expected: current `emacs-base` behavior remains available, and narrower capability tests prove the split.

- [ ] **Step 7: Commit**

Commit the Emacs split independently.

## Phase 5: Chat-Session Capability

### Task 5: Move Chat Semantics Out Of `e-chat`

**Files:**
- Create: `lisp/e-chat-session.el`
- Modify: `lisp/e-chat.el`
- Modify: `lisp/e-core.el`
- Modify: `test/e-chat-test.el`
- Add: `test/e-chat-session-test.el`

- [ ] **Step 1: Define chat-session action tests**

Add tests for actions that do not require rendering:

- submit prompt
- abort turn
- reset session
- rename session
- set model
- set reasoning effort
- build context preview data

- [ ] **Step 2: Implement `e-chat-session.el`**

Create action functions:

- `e-chat-session-submit`: validates non-empty prompt text and calls `e-harness-prompt-async`.
- `e-chat-session-abort`: calls `e-harness-abort`.
- `e-chat-session-reset`: calls `e-harness-reset`.
- `e-chat-session-rename`: calls `e-session-rename` through the harness session store.
- `e-chat-session-set-model`: calls `e-harness-set-session-model`.
- `e-chat-session-set-effort`: calls `e-harness-set-session-reasoning-effort`.
- `e-chat-session-context`: calls `e-harness-context`.

Also provide `e-chat-session-capability-create`.

Its `:actions` plist should expose the actions by stable names.

- [ ] **Step 3: Delegate from `e-chat`**

Replace direct calls in `e-chat-rename`, `e-chat-set-model`, `e-chat-set-effort`, `e-chat-show-context`, `e-chat-submit`, `e-chat-abort`, and `e-chat-reset` with calls to `e-chat-session`.

Keep all rendering, composer, keymap, progress, and block-navigation code in `e-chat.el`.

- [ ] **Step 4: Keep default shell setup working**

Ensure `e-chat--default-harness` activates a layer or direct capability set that includes `chat-session`, `base`, and Emacs capabilities.

- [ ] **Step 5: Verify**

Run:

```bash
eldev test test/e-chat-session-test.el test/e-chat-test.el
```

Expected: semantic tests pass without relying on buffer rendering; presentation tests still pass.

- [ ] **Step 6: Live reload**

Because this changes loaded Emacs Lisp behavior, run:

```bash
emacsclient --eval "(progn (require 'e-dev) (e-dev-reload \"/Users/dimitrivorona/projects/elisp/e\"))"
```

If unavailable, record the exact failure in the handoff.

- [ ] **Step 7: Commit**

Commit the chat-session extraction independently.

## Phase 6: Remove Layer-Owned Behavior Bridge

### Task 6: Make Layers Pure Presets

**Files:**
- Modify: `lisp/e-layers.el`
- Modify: `lisp/e-harness.el`
- Modify: `test/e-layers-test.el`
- Modify: `test/e-harness-test.el`
- Modify: `docs/architecture.md` only if wording must be updated after implementation

- [ ] **Step 1: Add tests that fail if layers own behavior**

Tests should assert new layer records use `:capabilities` and do not rely on direct `:tools` or `:context-providers`.

- [ ] **Step 2: Remove legacy layer contribution fields**

Remove or stop using direct layer-owned `:instructions`, `:tools`, and `:context-providers` once all current layers have been migrated.

- [ ] **Step 3: Update harness activation**

`e-harness-activate-layer` should activate capability contributions only. If direct capability activation is useful, expose `e-harness-activate-capability`, accepting a harness and a capability and registering that capability's tools/context/action contributions.

- [ ] **Step 4: Verify**

Run:

```bash
eldev test test/e-layers-test.el test/e-harness-test.el test/e-base-test.el test/e-emacs-base-test.el test/e-chat-test.el
```

Expected: all migrated layer and harness tests pass.

- [ ] **Step 5: Commit**

Commit removal of the bridge separately so regressions are easy to isolate.

## Phase 7: Session State Follow-Up

### Task 7: Add Branch/Summary/Compaction Records

**Files:**
- Modify: `lisp/e-session.el`
- Add or modify: `test/e-session-test.el`
- Modify: `docs/architecture.md` only if the implementation changes the documented storage contract

- [ ] **Step 1: Add tests for each durable record type**

Cover append and replay for:

- branch summary
- compaction summary
- current branch cursor

- [ ] **Step 2: Implement append-only records**

Add public functions:

- `e-session-append-branch-summary`
- `e-session-append-compaction`
- `e-session-set-current-branch`

Use the existing JSONL append pattern from current `message`, `activity-event`, `session-info`, and `messages-cleared` records: append one typed record to the session JSONL file, replay it in `e-session--replay-record`, update in-memory session fields, refresh derived fields when needed, and rewrite `index.json`.

- [ ] **Step 3: Verify**

Run:

```bash
eldev test test/e-session-test.el
```

Expected: new records round-trip through persistent store replay.

- [ ] **Step 4: Commit**

Commit session-state expansion independently.

## Phase 8: Context Retrieval Follow-Up

### Task 8: Add Evidence Retrieval Tools

**Files:**
- Create or modify: `lisp/e-evidence-tools.el`
- Modify: `lisp/e-core.el`
- Add: `test/e-evidence-tools-test.el`
- Modify capability/layer preset code only to activate these tools through a capability

- [ ] **Step 1: Add tests for retrieval behavior**

Cover:

- fetch message range
- fetch activity-event range
- fetch one tool result by turn/tool-call id if enough metadata exists

- [ ] **Step 2: Implement evidence read helpers**

Expose read-only helpers that operate on the session store and return structured snippets. Do not implement canvas-state in this task.

- [ ] **Step 3: Wrap helpers as a capability**

Create an `e-evidence-retrieval-capability-create` function that registers read-only retrieval tools.

- [ ] **Step 4: Verify**

Run:

```bash
eldev test test/e-evidence-tools-test.el test/e-session-test.el
```

Expected: retrieval tools return deterministic snippets from stored messages/activity events.

- [ ] **Step 5: Commit**

Commit evidence retrieval independently.

## Phase 9: Provider Interruption Follow-Up

### Task 9: Add In-Flight Cancellation

**Files:**
- Modify: `lisp/e-backend.el`
- Modify: `lisp/e-openai.el`
- Modify: `lisp/e-harness.el`
- Modify: `test/e-backend-test.el`
- Modify: `test/e-openai-test.el`
- Modify: `test/e-harness-test.el`

- [ ] **Step 1: Add fake-backend cancellation tests**

Test that aborting an active provider call invokes a backend cancellation path and settles the turn as cancelled.

- [ ] **Step 2: Extend backend contract**

Add a cancellable request handle or cancellation callback to the backend interface. Keep provider-specific mechanics inside backend adapters.

- [ ] **Step 3: Store active cancellation handles in harness state**

Update active turn entries so `e-harness-abort` can cancel queued timers and active backend calls.

- [ ] **Step 4: Implement OpenAI/Codex cancellation where feasible**

If the current `url.el` request path cannot reliably cancel in-flight requests, document the limitation in the test and keep the harness contract ready for adapters that can cancel.

- [ ] **Step 5: Verify**

Run:

```bash
eldev test test/e-backend-test.el test/e-openai-test.el test/e-harness-test.el
```

Expected: fake backend cancellation is proven; OpenAI adapter either cancels or has an explicit tested limitation.

- [ ] **Step 6: Live reload**

Run:

```bash
emacsclient --eval "(progn (require 'e-dev) (e-dev-reload \"/Users/dimitrivorona/projects/elisp/e\"))"
```

If unavailable, record the exact failure.

- [ ] **Step 7: Commit**

Commit cancellation separately.

## Phase 10: Final Alignment Pass

### Task 10: Remove Obsolete Names And Refresh Docs

**Files:**
- Modify: `docs/architecture.md`
- Modify: `docs/arch-align.md`
- Modify code/tests only if stale names remain

- [ ] **Step 1: Search for obsolete layer-owned language**

Run:

```bash
rg -n "layer-owned|layer context|active layer context|emacs-base.*atomic|capability layer|permission|audit" docs lisp test
```

Expected: matches are either intentionally historical/current-state notes or are updated.

- [ ] **Step 2: Search for old API usage**

Run:

```bash
rg -n "e-layer-tools|e-layer-context-providers|e-layers-context-messages|e-emacs-tools-register-defaults|e-base-tools-register-defaults" lisp test
```

Expected: old APIs are gone or limited to compatibility shims with tests.

- [ ] **Step 3: Run focused full validation**

Run:

```bash
eldev test
```

Expected: full ERT suite passes.

- [ ] **Step 4: Live reload**

Run:

```bash
emacsclient --eval "(progn (require 'e-dev) (e-dev-reload \"/Users/dimitrivorona/projects/elisp/e\"))"
```

If unavailable, record the exact failure.

- [ ] **Step 5: Commit final cleanup**

Commit final docs/name cleanup separately.

## Suggested Execution Order

1. Task 1: capability contract
2. Task 2: layer-to-capability activation bridge
3. Task 3: file/base split
4. Task 4: Emacs split
5. Task 5: chat-session capability
6. Task 6: remove layer-owned behavior bridge
7. Task 7: session state expansion
8. Task 8: evidence retrieval tools
9. Task 9: provider interruption
10. Task 10: final docs/name cleanup

Tasks 7-9 are follow-ups after the capability/layer/chat split. The core alignment is Tasks 1-6.

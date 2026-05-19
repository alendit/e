# MVP QA

## Slice 1: MVP Plan Artifact

**Scenario enabled:** The MVP work has a durable milestone definition that
captures the intended chat UI, layer model, context-provider behavior, Emacs
tool surface, save semantics, and verification gate.

An implementer can read `docs/mvp.md` and understand that the MVP has no
permission system, that `emacs-base` is the default layer, that automatic
context is visible-buffer metadata, and that buffer edits do not save until
`save_buffer` is called.

**Validation:** Review `docs/mvp.md` for the locked decisions:
`emacs-base`, visible-buffer context, no permissions, live-buffer mutation, and
separate `save_buffer`.

## Slice 2: Layer Context Providers

**Scenario enabled:** The harness can activate a layer that contributes tools,
instructions, and read-only context providers before a turn reaches the
backend.

A layer can register a tool through the harness tool registry and prepend
backend-neutral context messages ahead of the transcript. This lets future
layers add prompts, tools, skills, and context without changing the chat buffer
or OpenAI/Codex adapter.

**Validation:** `rtk eldev test test/e-layers-test.el
test/e-context-test.el test/e-harness-test.el`.

## Slice 3: Emacs Base Layer And Tools

**Scenario enabled:** The default `emacs-base` layer gives the model an
Emacs-aware operating surface.

With `emacs-base` active, the harness exposes these tools: `current_time`,
`list_buffers`, `read_buffer`, `write_buffer`, `edit_buffer`, `save_buffer`,
and `run_elisp`. It also contributes instructions that buffer edits are live
mutations and saving is explicit.

The visible-buffer context provider reports names, modes, file paths when
present, modified status, and visibility for buffers visible in windows. It
does not include full buffer contents by default.

**Validation:** `rtk eldev test test/e-emacs-base-test.el
test/e-emacs-tools-test.el test/e-tools-test.el`.

## Slice 4: Basic Chat Buffer

**Scenario enabled:** A user can open a basic `*e-chat*` buffer, submit a
prompt, see user/assistant/tool/error/lifecycle events rendered, reset the
session, and abort an active turn.

The presentation shell owns only the buffer, commands, keymap, status line, and
event rendering. It calls harness APIs for session creation, prompt submission,
wait/abort/reset behavior, and receives state through harness events.

**Validation:** `rtk eldev test test/e-chat-test.el test/e-test.el`.

## Slice 5: Architecture And Verification

**Scenario enabled:** The architecture document now reflects the MVP runtime
shape: harness-owned layers, context-provider prefix messages, `emacs-base`,
concrete buffer/elisp tools, and a thin chat presentation shell.

The project quality gate proves the MVP can be loaded and exercised through the
normal Eldev workflow and a clean batch reload.

**Validation:** `rtk eldev test`, `rtk eldev lint`, `rtk eldev compile`, and
batch `e-dev-reload`.

## Current MVP Scenario

With valid Codex-managed ChatGPT auth available at `CODEX_HOME/auth.json` or
`~/.codex/auth.json`, a user can now:

1. Run `M-x e-chat`.
2. Submit a prompt from `*e-chat*`.
3. Have the default harness activate `emacs-base`.
4. Send layer instructions and visible-buffer metadata into transcript-stack
   context.
5. Let the model call buffer tools or `run_elisp`.
6. Mutate live buffers with `write_buffer` or `edit_buffer` without saving.
7. Persist a file-backed buffer only when `save_buffer` is called.
8. See assistant messages, tool results, failures, starts, finishes, resets,
   and cancellations rendered in the chat buffer.

## Deferred

The MVP still intentionally defers durable persistence, permission and
confirmation controls, richer chat rendering, process execution tools,
harness self-modification, canvas-state context, multiple-layer product UX, and
interrupting a provider call already running inside Emacs.

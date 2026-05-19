# MVP: Chat UI, Emacs Base Layer, Buffer Tools

## Goal

Make `e` usable inside Emacs with the smallest chat-like agent surface: a basic
chat buffer, one default `emacs-base` layer, automatic visible-buffer context,
and Emacs-native tools for reading, writing, editing, saving buffers, and
running elisp.

Layers are harness-owned bundles. The chat buffer is presentation only: it
renders messages and events, then submits prompts to the harness.

## Scope

MVP should implement:

1. `elayers` as harness-side layer descriptors.
2. Layer-owned context providers.
3. One default active layer: `emacs-base`.
4. Visible-buffer metadata as automatic context.
5. Buffer read/write/edit/save tools.
6. Explicit elisp execution tool.
7. A basic `*e-chat*` presentation buffer.

MVP should not implement durable persistence, a permission/confirmation system,
canvas mode, multi-layer product UX, multi-agent delegation, or a rich chat UI.

## Layer Model

Add `elayers` as harness-side layer descriptors with these fields:

- `:id`
- `:name`
- `:instructions`
- `:tools`
- `:context-providers`
- `:skills`
- `:prompts`

Multiple active layers should be supported by shape, but MVP only ships and
exercises one layer: `emacs-base`.

Layer activation should affect the harness, not the presentation shell. Active
layers contribute instructions, context providers, and tool registrations before
a turn reaches the backend.

## Context Providers

Context providers are read-only context contributors supplied by active layers.

Provider shape:

```elisp
(e-context-provider-create
 :name 'visible-buffers
 :build (lambda (&key harness session-id turn-id)
          ...))
```

The build function receives harness/session/turn metadata and returns
backend-neutral context messages. Providers must not mutate buffers or session
state.

The transcript context strategy should merge layer context ahead of the normal
transcript:

```text
system instructions from active layers
system visible-buffer context from active providers
user/assistant/tool transcript
```

The OpenAI/Codex adapter continues to receive backend-neutral messages and
tools only.

## Emacs Base Layer

`emacs-base` is active by default for the MVP chat harness. It contributes:

- Emacs-aware custom instructions.
- A visible-buffer metadata context provider, enabled by default.
- Buffer tools.
- Elisp execution tool.

The visible-buffer provider should include buffer name, major mode, file path
when present, modified status, and window visibility. It must not include full
buffer contents by default.

## Tool Surface

MVP tools execute directly when active and called by the model. There is no
permissions or confirmation layer in MVP.

Tools:

- `list_buffers`: return buffer names, modes, file-backed status, modified
  status, and visibility.
- `read_buffer`: return full contents or a requested range from a named buffer.
- `write_buffer`: replace a named buffer's live contents; do not save.
- `edit_buffer`: exact old-text/new-text replacement in a named live buffer; do
  not save.
- `save_buffer`: persist a file-backed buffer using its existing
  `buffer-file-name`; fail clearly for non-file buffers.
- `run_elisp`: evaluate explicit elisp in Emacs and return printed result or
  surfaced error.

`write_buffer` and `edit_buffer` modify live Emacs buffers only. Persistence is
a separate `save_buffer` action. Autosave is not treated as a persistence
mechanism.

## Chat Buffer

Add a basic `*e-chat*` presentation buffer that can:

- create/select one session
- submit prompts
- render user, assistant, tool, error, started, and finished events
- show active-turn state
- abort the active turn
- reset the session

The chat buffer must not own provider-specific logic, tool policy, or context
assembly.

## Testing

Required tests:

- Layer activation registers `emacs-base` tools, instructions, and
  visible-buffer context provider.
- Active layer context is included in backend messages without provider-specific
  coupling.
- Visible-buffer context includes visible buffers and excludes hidden buffers by
  default.
- `write_buffer` and `edit_buffer` mutate temporary buffers without saving.
- `save_buffer` saves file-backed buffers and errors for non-file buffers.
- `edit_buffer` rejects missing text, duplicate matches, and no-op replacements.
- `run_elisp` returns values and surfaces evaluation errors.
- Chat UI smoke test with fake backend opens a chat buffer, submits a prompt,
  and renders the assistant response.

Verification commands:

```sh
rtk eldev test
rtk eldev lint
rtk eldev compile
rtk emacs -Q --batch -L . -L lisp -l e.el --eval "(progn (require 'e-dev) (e-dev-reload default-directory))"
```

## Architecture Constraints

- Harness owns active layers, lifecycle, policy, and turn setup.
- Context providers contribute backend-neutral context only.
- Presentation owns buffers, commands, keymaps, and rendering only.
- Concrete Emacs side effects live in tools.
- Provider auth and request/response mapping stay in backend adapters.

No MVP change should make UI rendering, Emacs side effects, or provider-specific
request logic part of context strategy policy.

# M2 QA

## Slice 1: Context Strategy Seam

**Scenario enabled:** A session can build backend-ready context through a named
strategy instead of letting the OpenAI adapter replay the transcript itself.

An agent prompt now flows through `transcript-stack`, which reads ordered
session messages and returns backend-neutral messages/options. A future
`canvas-state` strategy can replace this without changing provider auth,
presentation code, or the loop.

**Validation:** `rtk eldev test test/e-context-test.el`,
`rtk eldev test test/e-harness-test.el`, `rtk eldev test`, and batch
`e-dev-reload`.

## Slice 2: OpenAI/Codex Backend Adapter

**Scenario enabled:** A harness can use a ChatGPT-backed Codex adapter that
reads Codex-managed auth, builds a Responses-style request, and parses SSE into
the existing backend-neutral stream items.

The adapter owns `auth.json` lookup, access-token extraction, ChatGPT account-id
extraction from the JWT, request headers, request body shape, Codex endpoint
resolution, and stream parsing. Tests use injected transport and fixture tokens
so no real credentials are exposed.

**Validation:** `rtk eldev test test/e-openai-test.el`, `rtk eldev test`, and
batch `e-dev-reload`.

## Slice 3: Minimal Codex Harness Flow

**Scenario enabled:** Elisp code can create a Codex-backed harness, create a
session, submit a prompt, and read the persisted assistant message without a
presentation shell.

`e-openai-codex-create-harness` wires the Codex backend into the existing
harness API and stores the default model in backend-neutral turn options. This
is the smallest real-agent path before UI work.

**Validation:** `rtk eldev test test/e-openai-test.el`,
`rtk eldev test test/e-harness-test.el`, `rtk eldev test`, and batch
`e-dev-reload`.

## Slice 4: Tool Calling

**Scenario enabled:** A real model can receive backend-neutral function tool
definitions, request one, have the registry execute it, append a structured
tool result, and continue the turn with that tool result in context.

This slice originally used a temporary demo tool. The current MVP surface
removed that demo tool and uses the Emacs buffer/elisp tools provided by
`emacs-base`.

**Validation:** `rtk eldev test test/e-tools-test.el
test/e-emacs-tools-test.el test/e-harness-test.el test/e-loop-test.el`,
`rtk eldev test test/e-openai-test.el`, `rtk eldev test`, and batch
`e-dev-reload`.

## Slice 5: Async Active Turns

**Scenario enabled:** A caller can start a prompt asynchronously, observe the
active turn id, wait for settled state, cancel a queued turn, and receive a
structured error result when the provider fails.

This is intentionally narrow. It does not build a general scheduler and does
not interrupt a provider call already running inside Emacs. It establishes the
state contract needed by a future presentation shell and real streaming work.

**Validation:** `rtk eldev test test/e-harness-test.el`,
`rtk eldev test test/e-test.el test/e-harness-test.el`, `rtk eldev test`, and
batch `e-dev-reload`.

## Current End-To-End Scenario

With valid Codex-managed ChatGPT auth available at `CODEX_HOME/auth.json` or
`~/.codex/auth.json`, Elisp can now:

1. Create a Codex-backed harness with `e-openai-codex-create-harness`.
2. Register the `emacs-base` tools through focused Emacs capabilities.
3. Create a session through `e-harness-create-session`.
4. Submit a prompt with `e-harness-prompt` or `e-harness-prompt-async`.
5. Build context through `transcript-stack`.
6. Send a Codex Responses request through the OpenAI/Codex adapter.
7. Parse assistant deltas, assistant messages, function calls, and done events.
8. Persist user, assistant, and tool result messages in the session.
9. Observe lifecycle, message, tool, cancellation, and failure events.

## Deferred

M2 still intentionally defers rich presentation buffers, durable persistence,
canvas-state context, dangerous tools, file writes, process execution, elisp
evaluation, and harness self-modification.

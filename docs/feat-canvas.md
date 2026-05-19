# Canvas Context Mode

## Status

Deferred design idea. Do not implement this as part of the current core or the
first OpenAI backend slice.

This document records a context-management direction the architecture should
allow: a mode where a task canvas is the explicit semantic state of an agent
run, rather than the model inferring state from a replayed transcript.

## Motivation

The current core can support the classic context strategy: build each model
request from the stack of prior user messages, assistant messages, tool calls,
and tool results.

That baseline is useful and should be the first real context implementation.
It is also not the only useful shape for long-running work. Transcript-first
agents can lose clarity as turns accumulate, tool output grows, and compaction
rewrites history. A canvas-first loop would make the current task state an
explicit durable artifact that the model edits over time.

## Proposed Loop

Canvas mode treats the canvas as canonical semantic state:

```text
canvas revision N
+ latest user prompt
+ latest tool results
+ selected evidence from full log
-> model emits tool calls, user-facing message, or canvas edit request
-> runtime validates edit request against canvas revision N
-> accepted edit creates canvas revision N+1
-> raw turn, tool results, and canvas patch are appended to the full log
```

The normal model context is not the full transcript. It is the current canvas,
the latest prompt, recent observations, and explicit retrieved evidence. The
full log remains available through tools.

## State Model

The canvas should not become a summarized transcript. It should be a structured
state document with provenance.

Suggested initial sections:

```markdown
# Task Canvas

## Objective

## Current State

## Constraints

## Decisions

## Known Facts

## Open Questions

## Plan

## Working Set

## Recent Log Summary

## Full Log Pointers
```

Facts and decisions should cite evidence:

```text
[D3] Use device-code auth for headless CLI login.
Reason: avoids relying on a local browser callback.
Source: log:12, tool:auth-docs:4
Status: accepted
```

## Evidence And Retrieval

Canvas mode needs an append-only evidence log separate from the canvas:

- raw user prompts
- assistant messages
- tool calls
- tool results
- accepted canvas patches
- rejected canvas patches
- approvals and denials
- errors

The model should be able to request evidence with tools such as:

- `get-log-range`
- `search-log`
- `get-tool-result`
- `get-canvas-revision`
- `diff-canvas-revisions`

These tools should return explicit evidence snippets. The canvas may reference
log spans, but the canvas must not silently launder evidence into uncited facts.

## Patch And Concurrency Rules

Canvas edits should be versioned patch requests:

```elisp
(:canvas-id "task-123"
 :expected-revision 17
 :patch ...)
```

The runtime should reject stale edits when the expected revision does not match
the current canvas revision. This keeps concurrent tools, user edits, and agent
turns from overwriting each other silently.

The patch format is intentionally not chosen yet. Viable options include:

- markdown text patches for a first spike
- block-level patches with stable block ids
- structured section updates with provenance fields

The architecture should make that choice local to the canvas context strategy
and canvas store, not the backend adapter.

## Architecture Requirements

Canvas mode requires context construction to be an explicit strategy seam.

The harness and loop should eventually be able to select a context strategy per
session or turn:

- `transcript-stack`: backend context is built from prior messages and tool
  results.
- `canvas-state`: backend context is built from the current canvas revision,
  latest prompt, latest observations, and selected evidence.

The strategy owns context assembly and interpretation of model outputs related
to context state. The OpenAI backend should receive backend-neutral messages,
tools, and options; it should not know whether the context came from transcript
replay or canvas state.

The session store should remain the owner of durable state. A future canvas
store can either be a part of session state or a separate repository referenced
by session metadata. The full log must remain append-only evidence either way.

## Invariants

- Canvas is authoritative semantic state for canvas-mode sessions.
- Full log and tool results are append-only evidence.
- Canvas edits are validated versioned patches.
- Facts and decisions carry provenance.
- The model can retrieve full evidence through tools instead of receiving the
  entire log by default.
- Provider auth and provider payload mapping stay in backend adapters.
- Presentation shells may display and edit the canvas, but they do not own
  context strategy policy.

## Open Questions

- Should the first canvas be a markdown document or a block tree with stable
  ids?
- Should the model emit canvas patches directly, or should a second validation
  pass transform proposed edits into patches?
- Which canvas sections are required, and which are optional per task type?
- How much provenance is mandatory for facts and decisions?
- Should canvas revisions be stored beside session messages or in a separate
  repository?
- How should user edits to the canvas participate in optimistic concurrency?

## First Non-Implementation Step

Before implementing canvas mode, add a small context strategy contract around
the existing transcript-stack behavior. That keeps the first OpenAI/ChatGPT
backend from baking transcript replay directly into provider code and leaves a
clear seam for canvas-state experiments.

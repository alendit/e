# Core Slice QA Scenarios

This document maps each core implementation slice from `docs/core.md` to the
functionality it enables and the scenario a QA pass can exercise.

## Slice 1: Events

**Functionality:** The core can construct stable event plists with event id,
type, session id, turn id, payload, and timestamp fields.

**Scenario:** A future presentation shell or subscriber can receive a
`turn-started` or `message-added` event and inspect it without knowing which
backend, tool, or UI initiated the turn.

**Validation:** `e-events-test-make-event` proves the event shape.
`e-events-test-rejects-missing-type` proves invalid events fail early instead
of silently entering the core stream.

## Slice 2: In-Memory Sessions

**Functionality:** The core can create sessions, attach metadata, append
messages, read messages in insertion order, and surface missing-session errors.

**Scenario:** A harness can preserve a prompt/response transcript for
`session-1` while keeping session storage independent of buffers, files, and
future durable repositories.

**Validation:** Session tests cover creation, ordered append/read behavior, and
the missing-session error path.

## Slice 3: Backend Contract

**Functionality:** The core has a provider-neutral backend adapter shape and a
fake backend that streams backend-neutral items synchronously.

**Scenario:** Core behavior can be tested without OpenAI auth, network access,
or provider payloads. A fake backend can emit assistant deltas, final assistant
messages, tool calls, and done markers.

**Validation:** Backend tests prove fake stream item delivery and reject a
backend without a stream function.

## Slice 4: Tool Registry

**Functionality:** The core can register tools by name, execute structured tool
calls, and return structured tool results or structured missing-tool errors.

**Scenario:** A backend can request `echo`; the loop can route that call through
the registry and receive an `ok` result. A request for an unknown tool returns
an explicit error result instead of crashing or being swallowed.

**Validation:** Tool tests cover successful registration/execution and the
unknown-tool result shape.

## Slice 5: Agent Loop

**Functionality:** The loop can run one backend turn, emit lifecycle events,
append assistant messages, execute tool calls, append tool result messages, and
return a settled turn result.

**Scenario:** Given a user message and a fake backend response, the loop can
convert backend stream items into core messages and events without knowing
about sessions, UI buffers, or provider-specific response formats.

**Validation:** Loop tests cover assistant message persistence, `turn-started`
and `turn-finished` events, and tool-call execution through the registry.

## Slice 6: Harness Service

**Functionality:** The harness can create sessions, subscribe to events,
accept a prompt, write the user message, run a backend turn, persist the
assistant response, and surface an idle abort error.

**Scenario:** A presentation shell can call `(e-harness-prompt harness
"session-1" "question")` and then read the session transcript as `(user
assistant)` messages while receiving core lifecycle events.

**Validation:** Harness tests cover prompt-to-transcript behavior, event
subscription, and the explicit no-active-turn abort error.

## Slice 7: Core Entry Integration

**Functionality:** `(require 'e)` exposes the core harness API in addition to
the existing package scaffold commands.

**Scenario:** A user or presentation package can load `e` once and immediately
access `e-harness-create`, `e-harness-prompt`, and `e-harness-messages`
without requiring private implementation files manually.

**Validation:** Package smoke tests cover the existing public scaffold surface
and the new core harness API. The full ERT suite validates that exposing the
core surface does not regress earlier slices.

## Slice 8: Follow-Up Lifecycle

**Functionality:** The harness exposes `e-harness-follow-up` as the lifecycle
entry point for adding another turn to an existing session.

**Scenario:** A shell can submit a first prompt, then submit a follow-up prompt
against the same session and receive a transcript ordered as user, assistant,
user, assistant.

**Validation:** The follow-up harness test proves both turns append to the same
session in order.

## Slice 9: Reset And State Access

**Functionality:** The harness can reset a session transcript and report
settled session state, including session id, active turn id, and message count.

**Scenario:** A presentation shell can clear a conversation without owning
session internals, and it can ask the harness whether a session is idle and how
many messages it contains.

**Validation:** Harness and session tests cover transcript clearing and the
settled state plist for an idle session.

## Slice 10: Validation

**Functionality:** The package passes the project-level quality gates after the
core modules are integrated.

**Scenario:** A developer can run the normal Eldev workflow and batch-load the
package from a clean Emacs process, confirming the package is usable as a
normal Emacs Lisp package and not only inside the test harness.

**Validation:** `rtk eldev test`, `rtk eldev lint`, `rtk eldev compile`, and
the batch `(require 'e)` status command all pass.

## Current Core Scenario

The implemented core supports this end-to-end in-process flow:

```elisp
(require 'e)

(let* ((backend (e-backend-fake-create
                 :items '((:type assistant-message :content "answer")
                          (:type done :reason stop))))
       (harness (e-harness-create :backend backend)))
  (e-harness-create-session harness :id "session-1")
  (e-harness-prompt harness "session-1" "question")
  (e-harness-messages harness "session-1"))
```

Expected result:

```elisp
((:id "msg-user-1" :role user :content "question" :metadata nil)
 (:id "msg-1" :role assistant :content "answer" :metadata nil))
```

The exact ids are in-process counters, so QA should assert roles, content, and
ordering rather than fixed ids unless counters are reset inside the test.

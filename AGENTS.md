# AGENTS.md

## Project Direction

- `e` is an Emacs Lisp agent runtime inspired by pi-core. It should run inside Emacs and support live-configurable agents that can inspect, change, and extend Emacs state, including their own harnesses when explicit tool capabilities allow it.
- Keep the harness and presentation separate. The harness owns agent lifecycle, sessions, model routing, tool execution, resources, and architectural policy. Presentation shells own buffers, commands, keymaps, rendering, and user interaction.
- The first backend target is OpenAI API access through ChatGPT subscription auth where available, but the LLM backend must stay generic. Provider auth, request shapes, retry behavior, and model-specific features belong behind backend adapters.

## Interactive Development

- Strive to develop interactively against the running Emacs whenever the change affects runtime behavior, chat presentation, tools, backend requests, reload behavior, or buffer state. Batch tests are necessary but not sufficient for live agent workflows.
- After code changes, reload the affected modules into the running Emacs and call `e-dev-reload` through `emacsclient --eval` so existing `*e-chat*` buffers and harness state are refreshed. Do this proactively; the user should not need to run `M-x e-dev-reload` after each agent change.
- Use `emacsclient --eval` to inspect live state when debugging: active chat buffer contents, buffer-local harness/session values, active tool definitions, raw response buffers, diagnostics, and session messages. Prefer direct inspection of the running Emacs over guessing from source alone.
- For model/tool/backend changes, verify at least one live scenario in the running Emacs when feasible. Use temporary buffers for destructive tool checks, and avoid mutating user buffers unless the task explicitly calls for it.
- Keep live reload and inspection commands narrow and explicit. Do not rely on Doom-specific APIs for package behavior; use Doom only as the user's current Emacs distribution context.

## Architecture Guidance

Use this section to evaluate decomposition, dependency direction, side-effect placement, interface design, and testability.

### Hard Constraints

- Shape work packages around useful stopping points that move toward the final direction. If work stopped after the package, the project should be better off than not building it; this does not need to hold for every internal slice.
- Treat available information explicitly. Make likely changes easy, keep uncertain decisions local and reversible, and model stable behavior directly. Use small module, adapter, function, data-mapping, or config boundaries to keep uncertain decisions contained. Add an abstraction only when the contract is real, stable enough to name, and makes the next likely change cheaper.
- Give each behavior a clear owning component.
- Prefer application services over putting business logic into UI, transport, webhook, or tool handlers.
- Keep dependencies flowing from unstable code toward stable code.
- Keep core policy isolated from side effects.
- Do not mix unrelated domains into one coordinating component.
- Introduce abstractions only when they make a concrete likely change easier.
- Do not create shared interfaces that implementations can only satisfy by narrowing behavior, ignoring requirements, or throwing.
- Make compatibility expectations explicit. Keep legacy handling or legacy paths only when a real compatibility requirement exists; otherwise remove obsolete paths by default and mention that cleanup explicitly.
- Do not add fallback paths, broad defensive handling, or error swallowing unless that layer can make a correct domain decision; unexpected errors should surface to the top and fail in obvious ways.

### Required Self-Check For Design-Sensitive Changes

For design-sensitive changes, add these review questions in the final note, PR description, or equivalent handoff:

1. If this work package stopped here, would the project be better off than if it had not been built?
2. What final direction does this move toward?
3. Which decisions are likely to change, uncertain, or stable, and are uncertain ones local and reversible instead of hidden behind speculative abstraction?
4. What component owns this behavior, and why?
5. Does this change increase or reduce coupling?
6. Did any dependency start pointing the wrong way?
7. Could any side effect be moved outward into an adapter or shell?
8. Are the abstractions and interfaces semantically real?
9. What compatibility expectations apply, and were obsolete paths removed unless required?
10. What legacy code can we remove now?
11. Where are expected errors handled, and where do unexpected errors surface?
12. What tests prove the core behavior independently of the full system? If the design changed, would tests change narrowly, or would unrelated tests need rewrites?

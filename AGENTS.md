# AGENTS.md

## Project Direction

- `e` is an Emacs Lisp agent runtime inspired by pi-core. It should run inside Emacs and support live-configurable agents that can inspect, change, and extend Emacs state, including their own harnesses when explicit tool capabilities allow it.
- Keep the harness and presentation separate. The harness owns agent lifecycle, sessions, model routing, tool execution, resources, and architectural policy. Presentation shells own buffers, commands, keymaps, rendering, and user interaction.
- The first backend target is OpenAI API access through ChatGPT subscription auth where available, but the LLM backend must stay generic. Provider auth, request shapes, retry behavior, and model-specific features belong behind backend adapters.

## Runtime Vocabulary

- A capability action is a shell-facing semantic operation contributed by an active capability. Actions are not model-facing tools. Agents call them from `run_elisp` with `(e-actions-call 'capability :action ARGUMENTS)`.
- Use `e-tools-call` / `e-tools-call!` for model-facing tools and `e-actions-call` for capability actions. Both APIs resolve through the current harness/session context; pass `:harness` and `:session-id` explicitly only when no tool context is active.
- See `docs/references/runtime_concepts.org` for the durable definitions of harness, session, turn, capability, layer, tool, resource method, context provider, hook, action, shell, and backend adapter.

## Interactive Development

- Strive to develop interactively against the running Emacs whenever the change affects runtime behavior, chat presentation, tools, backend requests, reload behavior, or buffer state. Batch tests are necessary but not sufficient for live agent workflows.
- Reloading changed Emacs Lisp into the user's running Emacs is the agent's responsibility, not a user follow-up. After any change to `lisp/*.el`, `e.el`, or behavior that is already loaded in the user's Emacs, proactively run `e-dev-reload` through `emacsclient --eval` before claiming the work is ready.
- Use this reload shape from the repo root unless a task needs a narrower expression: `emacsclient --eval "(progn (require 'e-dev) (e-dev-reload \"/Users/dimitrivorona/projects/elisp/e\"))"`. Prefer direct `emacsclient` invocation; do not wrap it in `zsh -lc` unless shell features are actually required.
- If `emacsclient --eval` fails because no server/frame is available, report that exact failure in the final note instead of implying the live Emacs was refreshed. Do not tell the user to reload manually unless live reload was genuinely unavailable.
- After reloading, use `emacsclient --eval` to inspect live state when debugging: active chat buffer contents, buffer-local harness/session values, active tool definitions, raw response buffers, diagnostics, and session messages. Prefer direct inspection of the running Emacs over guessing from source alone.
- Final notes for behavior changes should say whether the running Emacs was reloaded and, when relevant, what live scenario was inspected. Batch test results alone do not prove the user's current Emacs session picked up the change.
- For model/tool/backend changes, verify at least one live scenario in the running Emacs when feasible. Use temporary buffers for destructive tool checks, and avoid mutating user buffers unless the task explicitly calls for it.
- Keep live reload and inspection commands narrow and explicit. Do not rely on Doom-specific APIs for package behavior; use Doom only as the user's current Emacs distribution context.


## How To

- Dev work plans and related dev work notes belong under `docs/feats/`; maintain `docs/feats/index.org` as the compact status index for coding-agent orientation.
- Use the dev work statuses `Planned`, `Ready`, `In-progress`, `In-review`, and `Done` exactly as described in `docs/references/dev_work.org`.
- When creating or updating feature `review.org` files, follow `docs/agents/review.org` for the expected structure, finding style, evidence, and completion notes.
- Tiny work, meaning small changes like trivial styling changes, belongs under `docs/feats/tiny/` and should be numbered there.
- Research notes belong under `docs/research/`.
- Bug reports belong under `docs/bugs/`. When a user creates a bug report, create a new directory under `docs/bugs/` containing `report.org` with a short description of what the user reported, then create `investigation.org` in the same directory with the results of investigating the report using both code and live access.
- See `docs/references/dev_work.org` for the current dev work, tiny work, research, and bug report conventions.

## Post-Change Checklist

- After finishing a coherent semantic slice, run the relevant verification, reload live Emacs when required, update the applicable docs/bug/feature ledger, and commit that slice before starting unrelated work unless the user explicitly asks not to commit.
- Keep commits semantic and scoped: stage only files that belong to the slice, leave unrelated dirty or untracked files alone, and split independent behavior, documentation, or guidance changes into separate commits.

## Architecture Guidance

Use this section to evaluate decomposition, dependency direction, side-effect placement, interface design, and testability.

### Hard Constraints

- Shape work packages around useful stopping points that move toward the final direction. If work stopped after the package, the project should be better off than not building it; this does not need to hold for every internal slice.
- Treat available information explicitly. Make likely changes easy, keep uncertain decisions local and reversible, and model stable behavior directly. Use small module, adapter, function, data-mapping, or config boundaries to keep uncertain decisions contained. Add an abstraction only when the contract is real, stable enough to name, and makes the next likely change cheaper.
- Place state where its meaning lives. Use the lowest durability and smallest owning scope that meet the real requirement. Persist durable facts, user intent, and stable configuration. Keep derived, high-churn, presentation, focus, selection, progress, and cache state near the runtime that owns it unless cross-restart behavior is required.
- Give each behavior a clear owning component with one cohesive responsibility and one primary reason to change. Split mixed policy, orchestration, UI, transport, persistence, provider, and tool concerns when they change for different reasons.
- Prefer application services over putting business logic into UI, transport, webhook, or tool handlers.
- Keep dependencies flowing from unstable code toward stable code. High-level policy and application code should depend on stable, domain-owned contracts at real adapter boundaries, not concrete UI, transport, persistence, provider, or tool clients.
- Keep core policy isolated from side effects.
- Do not mix unrelated domains into one coordinating component.
- Introduce abstractions only when they make a concrete likely change easier. Prefer direct design for stable behavior, and use adapters, strategies, data mappings, or config for real variation points without speculative extension layers.
- Keep interfaces and public contracts narrow and consumer-shaped. Do not force callers or implementations to depend on unused capabilities.
- Do not create shared interfaces that implementations can only satisfy by narrowing preconditions, weakening behavior, ignoring requirements, or throwing unsupported-operation errors.
- Make compatibility expectations explicit. Keep legacy handling or legacy paths only when a real compatibility requirement exists; otherwise remove obsolete paths by default and mention that cleanup explicitly.
- Aim for live reloadability on intended extension seams that change frequently: capability definitions and content, loaded layer definitions, and capability configuration should refresh through reload/sync paths. One-off cardinal changes to harness or core record shapes may require a full Emacs restart instead of compatibility shims that complicate steady-state code.
- Do not add fallback paths, broad defensive handling, or error swallowing unless that layer can make a correct domain decision; unexpected errors should surface to the top and fail in obvious ways.

### Required Self-Check For Design-Sensitive Changes

For design-sensitive changes, add these review questions in the final note, PR description, or equivalent handoff:

1. If this work package stopped here, would the project be better off than if it had not been built?
2. What final direction does this move toward?
3. Which decisions are likely to change, uncertain, or stable, and are uncertain ones local and reversible instead of hidden behind speculative abstraction? Are extension points tied to real variation instead of hypothetical futures?
4. What component owns this behavior, and why? Does it have one cohesive responsibility and one primary reason to change?
5. Does this change increase or reduce coupling? Are interfaces narrow, consumer-shaped, and free of unused capabilities?
6. Did any dependency start pointing the wrong way, especially from stable policy toward concrete adapters, providers, UI, transport, persistence, or tool clients?
7. Could any side effect be moved outward into an adapter or shell?
8. What state does this change add or mutate? Who owns it, how long should it live, how often does it change, can it be rebuilt, and why is this the smallest correct storage lifetime?
9. What is the performance impact of this change? Any regression must be justified from first principles by explaining why the correct approach fundamentally requires more work; do not accept regressions merely because the existing code structure makes them convenient.
10. Are the abstractions and interfaces semantically real, and can implementations substitute for each other without narrowed preconditions, weakened behavior, ignored requirements, or unsupported-operation errors?
11. What compatibility expectations apply, and were obsolete paths removed unless required?
12. What legacy code can we remove now?
13. Where are expected errors handled, and where do unexpected errors surface?
14. What tests prove the core behavior independently of the full system? If the design changed, would tests change narrowly, or would unrelated tests need rewrites?

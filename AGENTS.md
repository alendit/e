# AGENTS.md

## Project Direction

- `e` is an Emacs Lisp agent runtime inspired by pi-core. It should run inside Emacs and support live-configurable agents that can inspect, change, and extend Emacs state, including their own harnesses when explicit tool capabilities allow it.
- Keep the harness and presentation separate. The harness owns agent lifecycle, sessions, model routing, tool execution, resources, and architectural policy. Presentation shells own buffers, commands, keymaps, rendering, and user interaction.
- The first backend target is OpenAI API access through ChatGPT subscription auth where available, but the LLM backend must stay generic. Provider auth, request shapes, retry behavior, and model-specific features belong behind backend adapters.

## Runtime Vocabulary

- A capability action is a shell-facing semantic operation contributed by an active capability. Actions are not model-facing tools. Agents call them from `run_elisp` with `(e-actions-call 'capability :action ARGUMENTS)`. An action that settles immediately returns its result; a pending action returns a `work:` reference which must be passed to the top-level `await` tool to observe settlement and receive a bounded inline result. Never wait or poll inside `run_elisp`.
- Use `e-tools-call` / `e-tools-call!` for model-facing tools and `e-actions-call` for capability actions. Both APIs resolve through the current harness/session context; pass `:harness` and `:session-id` explicitly only when no tool context is active.
- See `docs/references/runtime_concepts.org` for the durable definitions of harness, session, turn, capability, layer, tool, resource method, context provider, hook, action, shell, and backend adapter.

## Interactive Development

- External agents such as Codex must validate Emacs Lisp changes outside the user's running Emacs by default. Use Eldev, batch Emacs, focused ERT, compile checks, and source inspection for proof. Do not make full `e-dev-reload` the default completion step.
- External agents must use `scripts/e-live-probe` for live-state diagnostics. Its supported probes are fixed, read-only, and bounded: `ping`, `selected`, `chat`, `windows`, and `symbol SYMBOL`. Do not use raw `emacsclient --eval` for diagnostic exploration, even when the intended result is scalar: evaluation failures can make Emacs serialize an unbounded live object before the client receives any output.
- If `scripts/e-live-probe` times out, do not retry it or send another Emacs request. Inspect the process from the OS with bounded `pgrep`, `ps`, or a short `sample`; do not signal or terminate the user's Emacs without explicit approval.
- If the user explicitly wants supported extension or presentation changes loaded into the running Emacs, ask before running `e-dev-reload` and explain that it may block the UI. The command reloads only already-loaded layer, default, shell, and developer modules. Prefer the direct shape `emacsclient --eval "(progn (require 'e-dev) (e-dev-reload \"/Users/dimitrivorona/projects/elisp/e\"))"` when it is explicitly requested.
- Final notes for Lisp behavior changes should state that repository-side validation passed and whether the running Emacs was left unchanged. When an extension reload or full Emacs restart would be needed to use the new behavior immediately, say so as an option rather than performing it automatically.
- Internal `e` agents modifying `e` should use lightweight dev actions where possible. Mark layer, default, shell, or developer-module changes with `mark-reload-required` scope `reloadable`. Mark core runtime, session, harness, Work, provider adapter, or record-shape changes with scope `restart`; `e-dev-reload` deliberately does not apply them. Do not interrupt active turns with either operation.
- Compilation is useful but optional. Run byte/native compilation as an explicit batch validation or background job, not as part of the ordinary live-update path.
- Do not rely on Doom-specific APIs for package behavior; use Doom only as the user's current Emacs distribution context.

## Graphical UI Testing and Debugging

- Use `bash e2e/run-graphical-tests.sh` for behavior that depends on graphical redisplay, window geometry, focus, viewport scrolling, timers, or workspace restoration. On macOS this uses a private transparent off-screen frame; on headless Linux it uses Xvfb. It does not connect to or reload the user's running Emacs.
- To capture an inspectable transition trace, set `E_GRAPHICAL_E2E_SCREENSHOT_DIR` and narrow the run with `E_GRAPHICAL_E2E_SELECTOR`, for example: `E_GRAPHICAL_E2E_SCREENSHOT_DIR=/tmp/e-chat-shots E_GRAPHICAL_E2E_SELECTOR=focused-composer bash e2e/run-graphical-tests.sh`.
- With screenshot capture enabled, the graphical helpers automatically record before/after pairs around keyboard input and asynchronous provider events, settled states after waits, and the state immediately before a wait timeout fails. Artifacts are ordered SVG screenshots plus `.state.el` sidecars; PNG copies are also written when `rsvg-convert` is installed.
- A graphical test can capture an arbitrary state with `(e-graphical-test-capture-state "after-open")`, or an arbitrary transition with `(e-graphical-test-capture-transition "workspace-switch" (lambda () ...))`. Pass a directory as the optional final argument when the environment variable is not set.
- Inspect the PNG or SVG visually and use the matching `.state.el` file for exact selected-buffer, window-edge, mode-line, point, and viewport values. Keep temporary captures outside the repository unless they are intentional test fixtures.
- Prefer these isolated artifacts over taking desktop screenshots of the user's live Emacs. Use the visible native test frame only when explicitly needed by setting `E_GRAPHICAL_E2E_NATIVE_VISIBLE=1`.


## How To

- Dev work plans and related dev work notes belong under `docs/feats/`; maintain `docs/feats/index.org` as the compact status index for coding-agent orientation.
- Use the dev work statuses `Planned`, `Ready`, `In-progress`, `In-review`, and `Done` exactly as described in `docs/references/dev_work.org`.
- When creating or updating feature `review.org` files, follow `docs/agents/review.org` for the expected structure, finding style, evidence, and completion notes.
- Tiny work, meaning small changes like trivial styling changes, belongs under `docs/feats/tiny/` and should be numbered there.
- Research notes belong under `docs/research/`.
- Bug reports belong under `docs/bugs/`. When a user creates a bug report, create a new directory under `docs/bugs/` containing `report.org` with a short description of what the user reported, then create `investigation.org` in the same directory with the results of investigating the report using both code and live access.
- See `docs/references/dev_work.org` for the current dev work, tiny work, research, and bug report conventions.

## Post-Change Checklist

- After finishing a coherent semantic slice, run the relevant repository-side verification, note whether a manual live reload is needed, update the applicable docs/bug/feature ledger, and commit that slice before starting unrelated work unless the user explicitly asks not to commit.
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

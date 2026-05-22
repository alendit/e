# gptel, gptel-agent, and e

## Purpose

This note compares `e` with two nearby Emacs AI projects:

- `gptel`: a mature Emacs LLM client and request library.
- `gptel-agent`: an agentic preset, tool, prompt, and sub-agent layer built on
  top of `gptel`.
- `e`: this repository's intended Emacs-hosted agent runtime and harness.

The comparison is not limited to the current `e` implementation. It includes
the direction recorded in `AGENTS.md`, `docs/architecture.md`,
`docs/mvp.md`, and `docs/feat-canvas.md`: a harness-first runtime with
provider-neutral backends, context strategies, layers, explicit side-effect
tools, presentation shells, durable session state, and eventually
canvas-oriented context management.

Upstream snapshots reviewed:

- `gptel`: `karthink/gptel` at `677eb955918ad5290432040bfa44d0ac4e05036d`
- `gptel-agent`: `karthink/gptel-agent` at
  `753e722778fcdefc165f049d27cbfea4fb909236`

## Short Answer

`gptel` is the strongest existing Emacs substrate for LLM interaction. It
already solves many hard product and provider problems: multi-backend requests,
streaming, chat buffers, ad hoc prompts from any buffer, tool calling,
confirmation, context attachments, model capabilities, MCP integration, media,
reasoning output, request introspection, and resumable file-backed chats.

`gptel-agent` is the closest existing thing to the near-term user experience of
`e`: it turns `gptel` into an autonomous agent through presets, tools, prompts,
sub-agents, skills, filesystem editing, Bash, web access, Emacs introspection,
and confirmation overlays.

`e` is still different in its intended center of gravity. It is not primarily a
chat client, not primarily a preset collection, and not just a bundle of tools.
Its goal is to make the Emacs-hosted harness itself the stable product
boundary: lifecycle, sessions, context strategies, layers, model routing, tool
execution, events, resources, and presentation-independent policy. That is the
niche to preserve if this project should remain distinct.

## Project Centers

### gptel

`gptel` describes itself as a simple LLM chat client for Emacs that is available
from any buffer. Its real scope is larger than that description suggests. It is
both a user-facing package and a programmable request substrate.

Important pieces:

- `gptel-request.el` owns the core request API, backend structs, request state,
  tools, finite-state-machine flow, curl/url transport, and backend generic
  methods.
- Provider adapters such as OpenAI, Anthropic, Gemini, Ollama, Bedrock, Kagi,
  GitHub Models, and OpenAI Responses map backend-specific payloads and streams.
- `gptel.el` owns the interactive buffer workflow, presets, menus, request
  inspection, tool UI integration, response insertion, and chat behavior.
- `gptel-context.el`, `gptel-org.el`, `gptel-rewrite.el`, and integration files
  add context, Org, rewrite, MCP, and workflow affordances.

The important design point is that `gptel` is a broad, mature Emacs-native LLM
client. It is optimized for being useful everywhere in Emacs and for letting
users build custom workflows using the existing request and tool APIs.

### gptel-agent

`gptel-agent` is explicitly a collection of tools and prompts to use `gptel`
agentically. Its README defines an agent as a bundle of system prompt, tools,
and the harness used to call the LLM, and notes that `gptel` calls such bundles
presets.

Important pieces:

- `gptel-agent.el` reads Markdown and Org agent specs, discovers agent skills,
  registers presets, manages default agent/planning presets, and opens a
  project-scoped `gptel` buffer with the agent preset loaded.
- `gptel-agent-tools.el` defines the main agent tools: Bash, web search, URL
  fetch, YouTube metadata/transcripts, filesystem glob/grep/read/write/edit,
  mkdir, insert, Flymake diagnostics, todo writing, skills, and sub-agent
  delegation.
- `gptel-agent-tools-introspection.el` defines Emacs introspection tools for
  symbols, load paths, features, manuals, library source, documentation, and
  Elisp evaluation.
- `agents/*.md` defines the default agent, planning agent, researcher,
  executor, and introspector prompts and tool scopes.

Its primary abstraction is still the `gptel` preset. That is pragmatic and
powerful: it gets provider support, UI, request lifecycle, tools, confirmations,
and session buffers from `gptel`. Its agent layer is mostly configuration,
tools, prompt design, skill discovery, and sub-agent orchestration.

### e

`e` is intended as an Emacs Lisp agent runtime inspired by pi-core. The current
implementation already contains a package entry point, backend-neutral loop,
in-memory sessions, context strategies, layers, an Emacs base layer, buffer and
Elisp tools, a ChatGPT/Codex backend adapter, and a basic chat presentation.

The intended direction is broader:

- Harness as the stable core service for lifecycle, sessions, model routing,
  queue/turn state, tool execution, events, resources, and policy.
- Presentation shells as replaceable UI layers that render events and submit
  user intent but do not own runtime policy.
- Context strategies as explicit, provider-neutral transformations from
  session state and resources into backend-ready messages.
- Layers as harness-owned bundles of instructions, tools, context providers,
  skills, and prompts.
- Backend adapters as provider-specific boundaries for auth, request shape,
  streaming, retry behavior, and model features.
- Execution tools as explicit side-effect boundaries for buffers, files,
  processes, Elisp evaluation, and eventually harness self-modification.
- Durable state, compaction records, branch summaries, and canvas-state context
  as future runtime concerns, not presentation conveniences.

## Feature Comparison

| Dimension | gptel | gptel-agent | e direction |
| --- | --- | --- | --- |
| Primary identity | Emacs LLM client and request library | Agentic preset/tool bundle for `gptel` | Emacs-hosted agent runtime and harness |
| User entry point | Any buffer, dedicated chat buffer, rewrite commands, programmatic `gptel-request` | `M-x gptel-agent`, `@gptel-agent`, presets in normal `gptel` buffers | Harness API plus presentation shells such as `e-chat` |
| Backend model | Mature multi-provider adapter set | Inherits `gptel` backends | Generic backend contract; first target is ChatGPT/Codex auth, but provider-specific behavior stays in adapters |
| Tool model | `gptel-make-tool`, global known tools, categories, confirmation/include flags, async support | Rich tool suite built with `gptel-make-tool` | `e-tools` registry with backend-neutral definitions and structured results; concrete Emacs tools live outside core |
| Agent configuration | Presets can bundle backend, model, system prompt, tools, context, hooks, parameters | Markdown/Org agent specs become presets; supports skills and sub-agents | Layers are harness-owned bundles; target includes tools, context providers, skills, prompts, and policies |
| Context model | Buffer/chat parsing plus context attachments and prompt transforms; presets can dynamically set context | Mostly prompt/preset-driven; sub-agents reduce context pollution by isolating tasks | Explicit context strategy seam: transcript stack now, canvas-state and evidence retrieval later |
| Session state | Regular buffers/files are the practical session artifact; chats can be saved and resumed | Inherits `gptel` session buffers; adds compaction command that rewrites buffer content | Session store is a runtime component; target includes durable messages, metadata, branch summaries, compaction, and canvas revisions |
| Presentation boundary | User-facing package and request library are intertwined by design but extensible | Uses `gptel` buffers and overlays | Presentation must stay a shell over harness events and lifecycle APIs |
| Side-effect boundary | Tools run local functions; confirmation can be per tool or global | Filesystem, Bash, Elisp, web, and delegation tools with confirmation previews | Side effects belong in concrete tools/adapters; core policy should stay pure and testable |
| Sub-agents | Possible through custom workflows/presets; not the base identity | First-class `Agent` tool invokes isolated specialized agents | Target direction likely supports multi-agent/delegation, but should be harness-level rather than only prompt-level |
| MCP | Existing integration through `mcp.el` | Inherits and can use MCP categories in presets | Not present yet; should be an adapter/tool/resource integration, not hardwired into core |
| Maturity | High | Young but usable and directly agentic | Early; architecture is clearer than product maturity |

## Architectural Differences

### gptel Is Substrate-First, Not Harness-First

`gptel` has the best existing Emacs substrate: backends, request handling,
streaming, tools, confirmation behavior, UI surfaces, context inclusion, and
programmatic APIs. It is already what many `e` backend and tool concerns would
otherwise have to rebuild.

However, `gptel` does not make a separate agent harness the central domain
object. Its core domain is an LLM request/chat workflow. Presets, tools, buffer
state, and hooks are flexible enough to express agentic behavior, but lifecycle
policy, durable semantic state, branch/compaction models, and context strategy
selection are not organized as a separate runtime layer in the way `e` is
aiming for.

This is not a weakness for `gptel`; it is a product choice. It is why `gptel` is
useful broadly and immediately.

### gptel-agent Is Agent UX, Not a New Runtime Core

`gptel-agent` is extremely close to the near-term behavior people may expect
from `e`: tools, edit/write operations, Elisp evaluation, web access,
sub-agents, planning/execution/research prompts, skills, and confirmations.

Its architecture is intentionally parasitic on `gptel` in the good sense: it
does not own provider adapters, request transport, buffer chat mechanics, or
general tool calling. It supplies the missing agent layer as presets, prompts,
tool definitions, and overlays.

That means `gptel-agent` can move faster on user-visible agent behavior. It also
means its durable state and context model are constrained by the `gptel` buffer
workflow unless it builds deeper runtime abstractions later.

### e Should Not Compete as Another Tool Bundle

If `e` tries to win by merely adding buffer reads/edits, `run_elisp`, web
search, Bash, and a chat buffer, it will be behind `gptel-agent` on day one.
That surface already exists upstream and benefits from `gptel` maturity.

The stronger direction is to treat those tools as the MVP proof that the
harness boundary works. The durable value should be:

- composable runtime services instead of only presets
- explicit context strategies instead of only transcript/buffer replay
- session and semantic state as first-class data
- evented lifecycle independent of any one UI buffer
- provider neutrality below the harness
- side effects and self-modification as explicit, auditable capabilities
- presentation shells that can be replaced without rewriting agent policy

## Where e Can Learn From gptel

### Backend Breadth

`gptel` already supports many providers and model capability variations. `e`
should not casually duplicate that matrix unless the runtime needs semantics
that cannot be expressed through `gptel-request`.

Possible paths:

- Use `gptel` as an optional backend adapter for normal API-key providers.
- Keep the current Codex/ChatGPT subscription adapter only where `gptel` does
  not expose the needed auth path or request shape.
- Define the `e-backend` contract narrowly enough that both a direct adapter
  and a `gptel` adapter can satisfy it.

### Tool Confirmation and Preview

`gptel` and `gptel-agent` have mature UX ideas around confirming tool calls,
including preview overlays for Bash, file edits, writes, inserts, Elisp eval,
and sub-agent tasks.

`e` should preserve its architectural rule that confirmation is not core loop
policy, but it should copy the product lesson: dangerous tool calls need a
previewable, inspectable, deferrable interaction. That belongs at the concrete
tool/presentation boundary.

### Presets and Dynamic Configuration

`gptel` presets are a useful precedent for layer-like bundles. They can include
backend, model, system prompt, tools, context, hooks, and dynamic evaluation.

`e` layers should be more runtime-owned than `gptel` presets, but they should
remain similarly ergonomic. The layer model should avoid becoming a verbose
internal-only abstraction that is harder to use than a `gptel` preset.

### Programmatic API

`gptel-request` is valuable because it is easy to call from arbitrary Elisp.
`e` should keep its public harness API similarly direct. A clean runtime does
not help if ordinary Emacs Lisp packages cannot embed or control it.

## Where e Can Be Distinct

### Harness Lifecycle as Domain Model

`e` should make lifecycle operations such as prompt, follow-up, abort, wait,
reset, session selection, layer activation, model routing, and event
subscription explicit stable APIs. These should remain independent of the chat
buffer.

This is the main distinction from `gptel-agent`, where the `gptel` buffer and
preset workflow are the natural center.

### Context Strategy Selection

The `canvas-state` note points to a real architectural difference. `gptel` and
`gptel-agent` are mostly transcript/buffer/context-attachment oriented. `e`
can make context strategy a first-class runtime decision:

- `transcript-stack`: send prior session messages and tool results.
- `canvas-state`: send a durable task canvas, latest prompt, recent
  observations, and selected evidence.
- future strategies: project memory, resource-indexed context, branch-aware
  summaries, or task-specific state documents.

The important rule is that provider adapters should receive backend-neutral
messages/options and should not know which context strategy produced them.

### Durable Semantic State

`gptel` can save and resume chat buffers, and `gptel-agent` can compact a
session by replacing buffer text with a summary. `e` can go further by treating
session state as structured runtime data:

- append-only evidence log
- mutable semantic state documents
- branch summaries
- compaction records
- model and reasoning-level changes
- tool result provenance
- canvas revisions and rejected edits

That would make long-running agents more inspectable than a compacted chat
buffer alone.

### Replaceable Presentations

`e-chat` should remain only one shell. The same harness should eventually be
usable from:

- a chat buffer
- a task canvas buffer
- project command menus
- minibuffer commands
- background jobs
- status dashboards
- direct Elisp APIs

This is close in spirit to `gptel` being available anywhere, but the dependency
direction is different: presentation consumes harness events instead of the
buffer being the primary session object.

### Harness Self-Modification

`e` explicitly wants live-configurable agents that can inspect, change, and
extend Emacs state, including their own harnesses when explicit tool
capabilities allow it. That is a sharper and riskier direction than normal
tool-assisted editing.

To keep this safe, self-modification should only happen through explicit tools
with recorded effects, narrow permissions, and visible state changes. This is
where `e`'s side-effect boundary matters.

## Strategic Options

### Option 1: Build e Fully Independently

Benefits:

- Complete control over harness semantics.
- No dependency on `gptel` internal request state or buffer model.
- Easier to enforce the harness/presentation split.

Costs:

- Rebuilds provider breadth, model capability tracking, tool-call edge cases,
  streaming, media, MCP, and UX polish already present in `gptel`.
- High risk of spending most time catching up rather than proving the distinct
  runtime idea.

This option only makes sense if `e` needs request semantics that are
fundamentally incompatible with `gptel`.

### Option 2: Use gptel as a Backend Adapter

Benefits:

- `e` keeps its own harness, sessions, context strategies, layers, and events.
- `gptel` supplies mature provider transport, model support, and tool protocol
  mapping where appropriate.
- Direct Codex/ChatGPT subscription auth can remain a specialized adapter.

Costs:

- Requires carefully mapping `e`'s backend-neutral messages/tools/results onto
  `gptel-request`.
- Some `gptel` behavior is buffer-oriented or global/buffer-local option
  oriented, so the adapter must avoid leaking presentation assumptions into
  `e` core.

This is likely the best medium-term path.

### Option 3: Reframe e as a gptel-agent Extension

Benefits:

- Fastest route to user-visible agent behavior.
- Reuses almost everything.
- Lower maintenance surface.

Costs:

- The harness-first architecture largely disappears.
- Context strategy, durable semantic state, event lifecycle, and presentation
  independence would become difficult to preserve.

This is attractive only if the project goal changes from runtime research to
practical Emacs agent usage.

## Recommendation

Do not compete with `gptel` as another LLM client, and do not compete with
`gptel-agent` as another preset/tool bundle.

Keep `e` focused on the harness-first runtime direction. Treat the current chat
UI and Emacs base tools as validation slices, not as the final differentiator.

The next architecture-sensitive work should explore one narrow interop path:

1. Define the minimum `e-backend` behavior needed from a generic provider
   adapter.
2. Spike a `gptel`-backed `e-backend` that takes `e` messages/options/tools and
   calls `gptel-request`.
3. Keep the existing Codex/ChatGPT adapter as a direct backend while evaluating
   whether it should remain separate.
4. Compare whether tool calls, confirmation, and stream events can round-trip
   without making `e-harness` depend on `gptel` presentation concepts.

If that works, `e` can stand on `gptel`'s provider maturity while preserving
its own runtime model. If it does not work cleanly, the failed adapter spike
will still clarify exactly which harness semantics require a direct backend
implementation.

## Design Self-Check

1. If this work package stopped here, would the project be better off than if it
   had not been built?

   Yes. The project now has a grounded comparison against the most relevant
   Emacs-native upstream projects and a clearer recommendation for preserving
   `e`'s distinct architecture.

2. What final direction does this move toward?

   A harness-first Emacs agent runtime that can optionally reuse mature
   provider/tooling substrates without collapsing into a UI-only or preset-only
   package.

3. Which decisions are likely to change, uncertain, or stable, and are
   uncertain ones local and reversible instead of hidden behind speculative
   abstraction?

   Stable: `e` should keep harness/presentation/backend/context boundaries.
   Uncertain: whether `gptel` should become a backend adapter, a dependency for
   selected providers, or only a reference implementation. The recommendation
   keeps this uncertainty local to a future adapter spike.

4. What component owns this behavior, and why?

   This document owns comparison and positioning only. Any future interop should
   be owned by a backend adapter, not by the harness core or presentation shell.

5. Does this change increase or reduce coupling?

   The document itself does not change runtime coupling. The recommendation is
   explicitly to test coupling through a narrow adapter rather than mixing
   `gptel` assumptions into core.

6. Did any dependency start pointing the wrong way?

   No code dependency was added. Future `gptel` usage should point from an
   adapter toward `gptel`, with core depending only on the `e-backend` contract.

7. Could any side effect be moved outward into an adapter or shell?

   Yes. The recommended interop path keeps provider transport in a backend
   adapter and confirmation/preview behavior near concrete tools and
   presentation.

8. Are the abstractions and interfaces semantically real?

   The comparison reinforces that backend adapters, tools, context strategies,
   layers, sessions, and presentations are real seams with existing change
   pressure, not speculative names.

9. What compatibility expectations apply, and were obsolete paths removed
   unless required?

   This is documentation only. No compatibility path was added.

10. What legacy code can we remove now?

   None from this comparison alone. If a future `gptel` backend adapter works,
   some provider-specific duplication may become removable or optional.

11. Where are expected errors handled, and where do unexpected errors surface?

   Not applicable to runtime behavior in this documentation change. Future
   adapter spikes should surface unexpected provider/tool mapping failures to
   the harness rather than swallowing them.

12. What tests prove the core behavior independently of the full system? If the
   design changed, would tests change narrowly, or would unrelated tests need
   rewrites?

   Existing `e` tests already prove core behavior with fake backends and tools.
   A future `gptel` adapter should have focused adapter tests that do not
   require rewriting harness, context, layer, or presentation tests.

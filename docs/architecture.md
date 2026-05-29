# Architecture

## Project Overview

`e` is an Emacs-hosted agent runtime. Its purpose is to let agents run inside
Emacs, inspect editor and project state, use explicit tools, and, when a
capability allows it, modify buffers, files, or runtime configuration.

The repository currently contains a usable chat-oriented runtime path: package
startup, a provider-neutral harness core, JSONL-backed session persistence,
capability-owned behavior bundles, layer presets, context assembly, a turn loop,
resource-operation tools, OpenAI-like backend adapters, presentation shells, live
reload support, and ERT coverage. Durable user data is primarily session state
under the user's Emacs directory plus optional project-local capability
configuration.

The architectural direction is capability-first. The harness owns lifecycle and
runtime records, capabilities own named behavior contracts, layers package those
capabilities, backend adapters own provider details, and shells own Emacs
interaction mechanics.

## Table Of Contents

- project overview: L3-L21
- architecture overview: L36-L80
- boundaries and invariants: L81-L116
- repository mapping: L117-L149
- components: L150-L348
- data and control flow: L349-L401
- public surfaces: L402-L432
- extension points: L433-L444
- testing and verification: L445-L463
- change management: L464-L470
- architecture discussion: L471-L512

## Architecture Overview

The normal runtime path starts in `e.el`. Package startup extends `load-path`,
loads the pure core, loads defaults, loads presentation shells, and then runs
startup hooks. Defaults register known layer specs and a lazy `:chat-default`
harness factory. Shells register manifests against the generic shell registry.

The harness is the stable application service. It creates sessions, tracks active
turns, activates layers, derives active capabilities, builds provider-neutral
context, dispatches a backend turn through the loop, executes tools, persists
durable records, and publishes events for presentation shells.

```mermaid
flowchart LR
    User["Emacs user"] --> Shells["Presentation shells"]
    Shells --> Actions["Capability actions"]
    Actions --> Harness["Core harness"]
    Defaults["Default harness and layer specs"] --> Harness
    Layers["Layer presets"] --> Caps["Capabilities"]
    Harness --> Layers
    Harness --> Context["Context strategy"]
    Caps --> Context
    Context --> Loop["Agent loop"]
    Loop --> Backend["Backend adapter"]
    Loop --> Tools["Tool registry"]
    Caps --> Tools
    Tools --> Effects["Buffers, files, processes, web, elisp"]
    Harness --> Store["Session store"]
    Store --> Shells
```

The major runtime parts are:

- Core substrate: `e-core`, harness, sessions, context, events, resources, hooks,
  tools, compaction, stores, and startup hooks.
- Capability system: behavior contracts that contribute instructions, context
  providers, tools, resources, hooks, actions, and configuration options.
- Layer system: stateless presets over capability sets and defaults.
- Defaults: lazy harness factories and built-in layer specs.
- Backend adapters: OpenAI-like provider profiles, request mapping, auth, SSE
  parsing, timeout, and cancellation mechanics.
- Presentation shells: chat, global starter, canvas, and layer-selection command
  surfaces over harness and capability APIs.
- Test/development support: Eldev/ERT tests and live Emacs reload helpers.

## Boundaries And Invariants

Confirmed current boundaries:

- `lisp/core/e-core.el` loads only core runtime modules. It does not load
  presentation shells, default harness factories, concrete provider adapters, or
  layer implementation modules.
- `e.el` is the package entry point. It loads core, defaults, shell modules, and
  runs startup hooks, so package startup is intentionally broader than `e-core`.
- `AGENTS.md` is the local architecture policy source. This document is the
  current-state navigation map and review artifact.
- Sessions are the durable runtime source of truth for messages, activity
  events, session events, turn options, branch summaries, compactions, metadata,
  and current branch state.
- Buffers, files, processes, browser sessions, and provider connections remain
  external state. Capabilities expose them through resources and tools rather
  than making the harness own them.

Invariants that should keep holding:

- Harness code must stay independent from buffers, windows, keymaps, rendering,
  provider auth, and concrete side effects.
- Presentation shells host commands, keymaps, rendering, and Emacs interaction;
  they must not own provider routing, session semantics, tool execution, or
  durable runtime state.
- Capabilities define semantic behavior. Layers activate capability sets but
  should not own behavior or durable state.
- Provider request shapes, auth files, headers, wire APIs, retries, streaming,
  timeouts, and cancellation handles belong in backend adapters.
- Context strategies build provider-neutral model input. They should not know
  about UI rendering or provider-specific payloads.
- Side effects cross the core boundary through resource methods, model-facing
  tools, backend adapters, or presentation commands.
- Expected domain errors should be handled where the owner has enough context;
  unexpected errors should surface to the caller or shell.

## Repository Mapping

- `AGENTS.md`: durable project direction, architecture constraints, interactive
  development rules, and review questions.
- `README.org`: compact current architecture overview for users and maintainers.
- `docs/architecture.md`: this current-state architecture map.
- `docs/arch-align.md`: completed capability-first alignment plan and remaining
  direction reference.
- `docs/core.md`, `docs/M2.md`, `docs/mvp.md`, and matching `*-qa.md` files:
  historical implementation and QA maps for delivered slices.
- `docs/feats/`, `docs/bugs/`, and `docs/research/`: tracked work packages,
  bug investigations, and research notes using repo-local conventions.
- `e.el`: package entry point, load-path setup, package startup, public smoke
  command, and `e-dev-reload` autoload.
- `lisp/core/`: provider-neutral runtime substrate. This area owns contracts and
  orchestration, and must stay free of presentation, defaults, and provider auth.
- `lisp/defaults/`: built-in layer specs and lazy default harness assembly.
- `lisp/layers/`: capability and layer implementations for base OS tools, live
  Emacs tools, harness support, agent context, evidence retrieval, web access,
  text-editing guidance, chat-session actions, and layer selection.
- `lisp/adapters/openai/`: OpenAI-like backend adapter and provider profiles.
- `lisp/shells/`: presentation shell manifests, commands, keymaps, buffers, and
  rendering.
- `lisp/dev/`: live Emacs reload helpers for interactive development.
- `test/`: ERT coverage for core contracts, adapters, layers, tools, sessions,
  defaults, and shells.
- `Eldev`: Eldev configuration over the built-in ERT test runner.

Dependency direction should remain visible in this layout: shells/defaults/adapters
depend on core contracts; core contracts do not depend on shells/defaults/adapters.
Layer directories may contain concrete tools because those tools are owned by the
capability vocabulary that activates them.

## Components

### Core Entry And Startup

`e.el` owns package-level startup. It adds source subdirectories to `load-path`,
requires the pure core, loads defaults and shell modules, runs `e-startup-run`,
defines `e-version`, exposes `e-status`, and autoloads `e-dev-reload`.

`lisp/core/e-startup.el` owns the two startup hooks: `e-startup-layer-hook` and
`e-startup-shell-hook`. Defaults register layer and harness specs on the layer
hook; shells register manifests and refresh shell state on the shell hook.

This split keeps the core loadable without provider or presentation code while
still allowing the package entry point to assemble a normal user-facing runtime.

### Core Harness

`lisp/core/e-harness.el` is the main application service. It owns harness
construction, active layer state, active-turn tracking, capability-derived tool,
hook, store, and resource registries, session creation, event subscription,
runtime event emission, context preparation, compaction, prompt submission,
follow-up, abort, wait, reset, model/effort session options, and public session
projections.

The harness depends on core contracts: sessions, context strategies, tools,
resources, hooks, capability config, layers, backend, loop, and stores. It does
not know which shell requested a turn or which provider backs the LLM. Its side
effects are delegated to session stores, backend request handles, tool request
handles, and resource/tool implementations.

Important source paths:

- `lisp/core/e-harness.el`
- `lisp/core/e-harness-registry.el`
- `lisp/defaults/e-default-harnesses.el`
- `test/e-harness-test.el`
- `test/e-harness-registry-test.el`
- `test/e-defaults-test.el`

### Sessions And Durable State

`lisp/core/e-session.el` owns durable runtime records. It supports in-memory
stores and persistent stores rooted at `(locate-user-emacs-file "e/sessions/")`.
Persistent sessions append JSONL records under `sessions/<id>.jsonl` and maintain
an `index.json` for recent-session metadata. The index can be loaded eagerly
while individual session transcripts are loaded on demand by
`e-session-load-session`.

Session records include `session`, `message`, `activity-event`, `session-info`,
`messages-cleared`, `branch-summary`, `compaction`, and `current-branch`. Session
identity uses generated ids and per-entry identity/parent links. Display titles
prefer explicit names, then first user-message summaries, then untitled
timestamps.

The store is append-only evidence plus derived mutable projections. Future
semantic state artifacts such as canvas revisions should not be hidden inside a
presentation shell; they should be session records or separate resources with
session-linked provenance.

### Capabilities, Resources, Hooks, And Tools

`lisp/core/e-capabilities.el` defines behavior contracts. A capability can
contribute instructions, context providers, model-facing tools, resource methods,
read-only `e://` resources, lifecycle hooks, shell-facing actions, configuration
options, and capability-local defaults.

Resource operations are generic contracts over URI schemes. `e-resources`
registers methods for operations such as `read`, `write`, and `edit`; the
harness exposes a model-facing operation tool only when active capabilities
provide at least one method for that operation. `e-store` exposes read-only
capability resources under `e://<capability>/<path>`.

`e-tools` owns backend-neutral function definitions, async tool execution,
request handles, structured tool results, and resource-usage metadata. Tool
lifecycle hooks are registered by capabilities through `e-hooks`, then invoked
by the harness around tool execution. Unexpected hook errors fail the turn rather
than silently removing protection.

Important source paths:

- `lisp/core/e-capabilities.el`
- `lisp/core/e-resources.el`
- `lisp/core/e-store.el`
- `lisp/core/e-tools.el`
- `lisp/core/e-hooks.el`
- `lisp/core/e-operations.el`
- `test/e-capabilities-test.el`
- `test/e-tools-test.el`
- `test/e-resources-test.el`

### Layers And Defaults

Layers are stateless presets over capabilities. `lisp/layers/e-layers.el` owns
known layer specs and factory resolution. `lisp/defaults/e-default-layers.el`
registers built-in specs for `e`, `e-dev`, `agents-std-context`, `harness-base`,
`os-base`, `emacs-base`, `web`, and `text-editing`.

`lisp/defaults/e-default-harnesses.el` registers the lazy `:chat-default` harness
factory. The default chat harness uses the OpenAI-like provider path, persistent
sessions, the `chat-session` capability, and default layer ids from
`e-default-chat-layer-ids`. Runtime layer enable/disable operations update that
custom option through the default harness sync path.

The layer-selection capability and shell provide operator commands for enabling,
disabling, and toggling registered layers without making the chat shell own
layer state.

### Context And Compaction

`lisp/core/e-context.el` owns provider-neutral context assembly. The current
strategy is `transcript-stack`, which builds messages from compacted session
state and prepends capability-provided context/instructions by priority.

`lisp/core/e-compaction.el` prepares compaction boundaries and summary prompts.
`e-harness-compact-session` runs a model-backed compaction, appends a compaction
record, and emits session events. `e-chat-session` exposes compaction as a
capability action and `e-chat` hosts it as a shell command.

Context providers are read-only. They may inspect session state, active
attachments, visible buffers, AGENTS/skill files, or resources, but they should
not perform provider-specific request shaping or concrete side effects.

### Agent Loop And Backend Adapter

`lisp/core/e-loop.el` owns one turn. It receives backend-neutral messages, tools,
options, and callbacks; streams assistant deltas and tool calls; executes tools;
feeds tool results back into the message list; re-queries the backend when
function calls require follow-up; and emits lifecycle events to the harness.

`lisp/core/e-backend.el` defines synchronous and asynchronous backend contracts
plus cancellable request handles. The OpenAI adapter in `lisp/adapters/openai/`
implements provider profiles, model/reasoning defaults, Codex auth-file loading,
token-auth profiles, Responses and Chat Completions request mapping, SSE parsing,
HTTP timeouts, raw diagnostics, and cancellable `url-retrieve` requests. Injected
request functions remain queued-only cancellable test seams.

Adding a provider should be an adapter change. It should not require changing
the chat shell, session store, or harness lifecycle policy.

### Execution Capabilities

The base OS capabilities live under `lisp/layers/base/`. `os-base` packages
`base-guidance`, `file-inspection`, `file-mutation`, and `shell-process`.
`file://` read/write/edit methods enforce the resource operation contracts.
The `bash` tool runs process commands and streams output through a file-backed
collector so large output can be represented by bounded previews plus `tmp://`
resources when harness support is active.

The Emacs capabilities live under `lisp/layers/emacs/`. `emacs-base` packages
awareness, buffer read/edit resources, elisp evaluation, and selection context.
`buffer://` methods mutate live buffers; `save_buffer` is the explicit action
that persists file-backed buffer contents.

Harness support capabilities live under `lisp/layers/harness/`. They own
session-scoped `tmp://` resources and tool-output truncation hooks.

Optional capability layers include:

- `agents-std-context`: AGENTS.md and configured filesystem skill context plus
  skill resources.
- `e`: runtime self-management commands such as layer selection and context
  inspection.
- `web`: `web_search`, `web_fetch`, `web_browser`, and web reference resources.
- `text-editing`: progressive guidance resources for Simply Annotate workflows.
- `evidence-retrieval`: read-only tools for durable session messages, activity
  events, and individual tool results.

### Presentation Shells

`lisp/shells/e-shells.el` defines the shell manifest registry. A manifest names
a shell id, metadata, required/optional capabilities, commands, and keymaps. The
registry is intentionally narrow: it is discovery, not a shell lifecycle or
dependency-resolution framework.

Current shells:

- `e-chat`: session chat buffer, composer, rendering, block navigation, tool
  output views, context preview, compaction command, overview/sidebar, resume,
  switch, rename, model/effort commands, abort/reset, and source-reference
  capture. It requires `chat-session` and talks to the harness through public
  APIs and capability actions.
- `e-chat-starter`: global one-shot contextual prompt shell over a chat session.
- `e-canvas`: commands that create or attach live buffer/file context as
  `chat-session` attachments, including a primary canvas attachment.
- `e-layers-shell`: operator commands for known layer selection.

Shell instances are implementation details. `e-chat` uses buffer-local state,
markers, overlays, timers, and subscriptions to render a session, but those are
not generic runtime state and should not become semantic owners.

### Live Development

`lisp/dev/e-dev.el` owns live reload support for this repository. Repo policy
requires runtime-affecting changes to be reloaded into the user's running Emacs
with `e-dev-reload` when available, then inspected with focused live probes.

This reload helper is developer support rather than runtime policy. It is loaded
through `e.el` and autoloaded for interactive use, not through the pure core.

## Data And Control Flow

Normal chat turn:

```mermaid
sequenceDiagram
    participant User as Emacs user
    participant Chat as Shell
    participant Cap as chat-session
    participant H as Harness
    participant S as Session store
    participant C as Context strategy
    participant L as Loop
    participant B as Backend adapter
    participant T as Tool registry

    User->>Chat: submit prompt
    Chat->>Cap: submit action
    Cap->>H: prompt async
    H->>S: append user message
    H->>C: build context
    C->>S: read durable records
    C-->>H: messages and options
    H->>L: start turn
    L->>B: stream request
    B-->>L: deltas and tool calls
    L->>T: execute tool call
    T-->>L: structured result
    L-->>H: events and messages
    H->>S: append durable records
    H-->>Chat: render events/projections
```

Abort flow cancels what the harness currently owns. A queued turn cancels its
timer; an active backend/tool request is asked to cancel through its request
handle; an open tool call receives a durable cancelled tool result; the turn
emits `turn-cancelled`.

Layer selection flow stays outside presentation semantics. A shell command calls
the `layer-selection` capability action, which creates or removes a registered
layer on the target harness. The default harness sync path records changes back
to `e-default-chat-layer-ids` for the default chat harness.

Live context attachment flow is session metadata, not transcript history. Canvas
or file/buffer attachments are stored on the session, and the `chat-session`
context provider reads current live content on each turn. Unsaved live buffer
contents win over disk reads.

Tool output protection flow runs through hooks. Tool results are normalized into
one structured result shape, post-tool hooks can replace large content with
bounded previews and `tmp://` references, and durable activity payloads are
compacted before being emitted and persisted.

## Public Surfaces

The main public surfaces are:

- Package entry: `(require 'e)`, `e-version`, `e-status`, and `e-dev-reload`.
- Harness API: `e-harness-create`, session creation/list/projection accessors,
  prompt/follow-up/abort/wait/reset/compact operations, model/effort session
  options, layer activation, and event subscription.
- Harness registry: named live harness instances and lazy factories through
  `e-harness-registry-*`.
- Session API: session creation, append/load/list, metadata, turn options,
  branch summaries, compactions, current branch, and display titles.
- Capability API: `e-capability-create`, contribution accessors, contribution
  registration helpers, actions, config options, and skill construction helpers.
- Resource/tool API: `e-operation-*`, `e-resources-*`, `e-store-*`,
  `e-tools-*`, and `e-session-tmp-*`.
- Layer API: layer specs, layer creation from registered specs, default layer
  registration, and layer-selection actions.
- Backend API: `e-backend-*`, `e-openai-backend-create`,
  `e-openai-create-harness`, and Codex compatibility wrappers.
- Shell API: `e-shell-*`, `e-chat-shell`, `e-chat-starter-shell`,
  `e-canvas-shell`, and `e-layers-shell`.
- Interactive commands: chat session creation/resume/switch/overview/sidebar,
  prompt submission, abort/reset, rename, model/effort, context preview,
  compaction, response/block/tool-output navigation, canvas attachment, global
  starter, and layer enable/disable/toggle.

Exhaustive function inventories should stay in source and tests. Architecture
depends on the ownership of these surfaces, not on duplicating every command name
here.

## Extension Points

Established extension points are backend adapters, OpenAI-like provider profiles,
context strategies, context providers, capabilities, layer presets, resource
operation methods, `e://` resources, lifecycle hooks, model-facing tools,
session stores, startup hooks, and presentation shell manifests.

Inferred but not yet mature extension points are richer context-state strategies,
permission/audit policy, harness self-modification tools, and generic shell
instance lifecycle. They should not receive broad abstractions until a second
real implementation gives the contract stable semantics.

## Testing And Verification

The project uses Eldev with Emacs' built-in ERT runner. The current tree has
focused tests for package exposure, events, sessions, backend contracts, OpenAI
mapping, context construction, compaction, capabilities, capability config,
resources, stores, hooks, tools, base/Emacs/web/evidence capabilities, layers,
defaults, harness behavior, registry behavior, loop behavior, chat-session
actions, chat presentation, starter/canvas shells, and development reload.

Core behavior is testable with fake backends, injected transports, in-memory
stores, temporary persistent stores, fake tools, and capability fixtures. Adapter
tests cover provider request/stream mapping and concrete side effects. Shell
tests should keep proving command wiring and rendering against harness events
rather than reimplementing harness tests.

Runtime-facing changes still require live Emacs reload and focused live probes
because batch-green Emacs Lisp does not prove the user's current Emacs process
has the new definitions.

## Change Management

Update this document when any of these move: core/presentation boundary, default
harness assembly, layer/capability ownership, context strategy contract, session
record schema, tool lifecycle semantics, backend adapter contract, resource URI
semantics, shell manifest shape, or public harness/session command surface.

## Architecture Discussion

Against the repo guidance, the current architecture is mostly aligned with the
target decomposition:

- Ownership is clear in the main path. Harness owns lifecycle and runtime
  records; sessions own durable state; capabilities own behavior; layers package
  capabilities; adapters own provider and side-effect details; shells own Emacs
  presentation.
- Dependency direction is mostly correct. Core modules depend on stable local
  contracts, while defaults, shells, layer implementations, and OpenAI adapters
  depend outward on core contracts.
- Side effects are largely outside the core. File, buffer, shell, web, elisp, and
  provider operations sit behind tools/adapters/resource methods. Session
  persistence is the core's intentional durable side effect.
- Interfaces are now semantically real. The backend, session, tool, resource,
  hook, capability, layer, context, harness, and shell-manifest contracts each
  have current consumers and tests.

Confirmed gaps:

- Permission and audit policy are named in the architecture but not implemented
  as a first-class capability/tool gate.
- Canvas support is currently live context attachment plus shell commands, not a
  separate versioned canvas-state context strategy.
- Harness self-modification is still architectural direction, not an exposed
  tool surface.
- Shell manifests are discovery records only. Generic shell lifecycle,
  capability matching, and shell-to-shell handoff are intentionally absent.
- The synchronous backend helper still publishes a non-cancellable request
  marker; the normal async `url-retrieve` path returns a cancellable request.

Delta to the architectural vision:

- The project has moved from scaffold toward a working capability-first runtime.
  The OpenAI/Codex path, persistent sessions, context strategy, tool lifecycle,
  default harness, shell manifests, live context attachments, web/text-editing
  layers, and cancellation hooks are implemented.
- The remaining vision work is not more generic structure by default. It is
  concrete policy and state work: permission/audit records, richer context-state
  artifacts, explicit self-modification tools, and only then broader shell
  lifecycle semantics if multiple shells require the same contract.

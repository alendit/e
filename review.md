# Review: `eeabcf6` Skills over Capabilities Implementation

## Summary

The latest commit implements the main direction of `docs/feats/01-skills-over-capabilities/plan.md` well. Skills are no longer a harness-level catalog: `e-harness.el` no longer requires `e-skills`, `e-harness--skill-catalog-context-messages` was removed, and `e-harness-context` now delegates only to normal capability context/instruction contribution. The new `e-skill-spec` plus `e-capability-with-skills-create` API constructs an ordinary `e-capability` whose instructions contain compact skill references and whose `:resources` register full bodies as normal read-only `e://` resources. This matches the intended progressive-discovery path: initial instructions advertise URIs; the existing `read` tool loads details on demand.

The multi-skill builder is present and covered by tests. `e-capability-with-skills-create` accepts `:skills`, appends a single generated preamble, preserves normal capability fields (`:tools`, `:resource-methods`, caller `:resources`, `:context-providers`, and `:actions`), and appends one generated resource provider after caller-provided resources. `test/e-skills-test.el` now checks multiple skills, resource ordering, dynamic readers, explicit paths, validation, no full bodies in generated instructions, no empty preamble without skills, and preservation of existing resources. Harness tests were updated to verify the preamble enters context through normal capability instructions and that `read` can load skill bodies. The full suite passes locally: `eldev test` ran 301 tests with 300 expected passes and 1 expected skip.

## Plan Compliance

The implementation satisfies the plan's core architectural acceptance criteria. The harness does not scan the store for skill metadata or inject a separate skill catalog. Generated skill bodies are ordinary `e-store` entries and become readable only because `e-harness-resources` adds the existing `e-store-resource-method` when store entries exist. The docs were updated in `docs/architecture.md` and `docs/mvp.md` to describe skills as builder sugar rather than a first-level runtime construct. The capability object also stays construction-oriented/idempotent: the builder returns a new `e-capability` and does not mutate an existing capability.

Compatibility was handled conservatively. `e-skill-create` is now an obsolete alias to `e-skill-spec-create`, and `e-skills-register` remains as a compatibility helper that registers a spec into a store or delegates function providers. The obsolete catalog functions (`e-skills-list`, `e-skills-catalog-text`) are removed, and a repository search finds no remaining non-plan references to them. This is aligned with the plan's preference to remove the harness scanning behavior and keep any old names thin/compatibility-only.

## Issues and Risks

One issue is that `e-skill-spec-create` allows both `:content` and `:reader`, but registration passes both through to `e-store-register`. `e-store-read-entry` always prefers the reader when present, so this works, but the contract is implicit. Either document that `:reader` takes precedence over `:content`, or reject specs that provide both. The plan said `:content` or `:reader`; making precedence explicit would avoid confusion.

Another small mismatch is validation for explicit paths. The plan says `:name` has no `/` if it is used in the default path. The implementation allows slashes in `:name` when `:path` is explicit, and tests cover this. That is reasonable, because the name becomes display metadata only, but it means the generated preamble can contain names like `grouped/name`. If that is intentional, it should be documented in `e-skill-spec-create`; if not, keep names slash-free regardless of path.

The implementation uses private `e-store--normalize-path` from `e-skills.el`. That is acceptable inside this package, but it does couple the skill builder to an internal store helper. If this API becomes public/stable, consider adding a public store path validation/helper or duplicating the small validation locally to avoid relying on double-dash internals.

Tests are strong for the new behavior, but there are two useful gaps. First, add a test that `:skill-heading` overrides the default heading. Second, add a test that non-resource capability fields are preserved, especially `:tools`, `:resource-methods`, `:context-providers`, and `:actions`; the current tests explicitly cover caller `:resources` but not the other fields. These are not blockers, but they would lock down the builder's passthrough contract from the plan.

## Recommendation

Overall, this is a good implementation of the plan. I would accept the architectural refactor as-is and follow up with small hardening changes: document or enforce `content`/`reader` precedence, document explicit-path name rules, add tests for `:skill-heading` and passthrough fields, and consider avoiding the private `e-store--normalize-path` dependency if the builder API is meant to be externally stable.

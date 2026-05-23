# Write Creates Parent Paths Plan

## Goal

Make parent/target creation an invariant of the model-facing `write` operation. The model should not need to reason about different creation behavior for different writable URI schemes. If a scheme is advertised under the `write` tool, writing should mean:

> Write complete content to this URI. Create missing parent paths and the target resource; overwrite existing content.

Schemes that cannot satisfy that contract should not advertise `write` for the affected URI class.

## Current Context

The resource system already centralizes model-facing write behavior through `e-operation-write` and active resource methods:

- `e-operation-write` defines the generic `write` tool name, description, parameters, and dispatcher.
- `e-harness--resource-operation-description` combines the generic operation description with active URI scheme descriptions.
- File resources already appear to create missing parents, as covered by `e-base-tools-test-write-file-creates-parents-and-overwrites`.
- `e://` resources are currently read-only through `e-store-resource-method`, so they do not advertise `write`; future writable `e://` methods should follow the invariant naturally because in-memory keyed stores have no filesystem parent directories.
- `buffer://` writable behavior needs explicit review: if the buffer write method advertises `write`, either it should create a missing live buffer or its registered writable URI class should be narrowed so the invariant is not misleading.

## Target Model-Facing Tool Description

Update the generic write operation description to be short and invariant-oriented:

```text
Write complete content to a URI-addressed resource. For every URI scheme listed below, write creates missing parent paths and the target resource, or overwrites existing content.
```

The generated tool description should then read like:

```text
Write complete content to a URI-addressed resource. For every URI scheme listed below, write creates missing parent paths and the target resource, or overwrites existing content.

Active URI schemes:
- file://<path> Workspace text files.
- buffer://<buffer-name> Live Emacs buffers. Writes mutate buffers but do not save file-backed buffers.
```

Scheme-specific descriptions should focus on scope and side effects. They should not need to restate parent creation unless there is a scheme-specific nuance.

## Write Operation Invariant

For any resource method registered for `e-operation-write`:

1. `write` replaces the complete resource content.
2. If the target resource does not exist, `write` creates it.
3. If the URI has meaningful parent containers, `write` creates missing parents.
4. Creation must remain inside the capability/resource method's allowed scope.
5. Invalid intermediate paths fail clearly. For example, a filesystem parent segment that already exists as a file should error.
6. If the scheme cannot safely provide this behavior, it should not register a write method for that URI class.

Keep `edit` stricter. `edit` applies exact replacements to existing text and should not create missing files, buffers, or parent paths.

## Implementation Steps

1. Update `e-operation-write` in `lisp/core/e-operations.el`.
   - Replace the description with the invariant-oriented wording above.
   - Keep the parameter shape unchanged: `:uri` and `:content`.

2. Audit existing writable resource methods.
   - File write method in `lisp/layers/base/e-base-tools.el`: confirm it creates missing parent directories and overwrites existing files while staying inside the configured root.
   - Buffer write method in `lisp/layers/emacs/e-emacs-tools.el`: decide whether `write buffer://<buffer-name>` should create a missing live buffer. Prefer making it create the buffer, because that matches the invariant and preserves a simple model contract. Saving file-backed buffers remains a separate `save_buffer` action/tool.
   - Any other write methods: either implement target/parent creation or stop advertising `write` for unsupported URI classes.

3. Adjust scheme descriptions where needed.
   - Keep file scheme wording concise, e.g. `Workspace text files.`
   - Keep buffer scheme wording concise and accurate, e.g. `Live Emacs buffers. Writes mutate buffers but do not save file-backed buffers.`
   - Avoid descriptions that imply writes only work for pre-existing resources if the method now creates missing targets.

4. Add or update tests.
   - Operation description test: assert the generated `write` operation description includes the create/overwrite invariant.
   - File resource tests: keep/strengthen coverage that writing `file://new/dir/file.md` creates `new/dir` and writes the target.
   - File safety tests: assert attempts to write outside the configured root still fail, and invalid intermediate paths surface errors.
   - Buffer write tests: if adopting creation, assert `write buffer://new-buffer` creates a live buffer with the provided content and does not save anything to disk.
   - Buffer overwrite tests: assert writing an existing buffer replaces the full buffer content.
   - Edit tests: assert `edit` still fails for missing file/buffer targets and does not create parents.

5. Update docs.
   - Update `docs/architecture.md` resource operation language to mention the `write` invariant.
   - Update any user-facing resource/tool documentation that currently describes `write` as replacement only.

## Acceptance Criteria

- The model-facing `write` tool description states the create-parent/create-target/overwrite invariant succinctly.
- Every active writable URI scheme conforms to that invariant or no longer advertises `write`.
- `file://` writes create missing parent directories and target files inside the configured root.
- `buffer://` write behavior is made consistent with the invariant or explicitly removed/narrowed from writable exposure.
- `edit` behavior remains unchanged and does not create missing resources.
- Tests cover operation-level wording and per-scheme behavior.

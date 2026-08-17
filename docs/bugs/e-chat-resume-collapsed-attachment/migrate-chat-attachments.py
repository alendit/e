#!/usr/bin/env python3
"""Migrate historical chat attachment metadata to the canonical session shape.

This is an offline, one-off store migration.  It scans the complete selected
store before writing, defaults to a dry run, creates sibling timestamped
backups, translates paired checkpoint offsets to rewritten journal record
boundaries, and atomically replaces only changed files.  Stop Emacs before
apply mode so its session writer cannot race the migration.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import shutil
import stat
import tempfile
from collections import Counter
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any


LEGACY_KEY = "context-attachments"
REFERENCE_KEY = "context-references"
OWNER_KEY = "chat-session"
ATTACHMENTS_KEY = "attachments"
FLATTENED_KEYS = {
    "uri", "label", "buffer-name", "file", "mode", "id", "canvas"
}


def fail(message: str) -> None:
    raise SystemExit(f"migrate-chat-attachments: {message}")


@dataclass
class StoreFile:
    path: Path
    kind: str
    root: Any
    records: list[dict[str, Any]]
    original: str
    raw_lines: list[str] | None = None
    selected_indices: set[int] | None = None
    changed_indices: set[int] = field(default_factory=set)
    structural_changed: bool = False


def read_jsonl(path: Path) -> StoreFile:
    original = path.read_text(encoding="utf-8")
    raw_lines = original.splitlines()
    records: list[dict[str, Any]] = []
    for number, raw in enumerate(raw_lines, 1):
        if not raw:
            fail(f"{path}:{number}: blank journal line")
        try:
            record = json.loads(raw)
        except json.JSONDecodeError as error:
            fail(f"{path}:{number}: invalid JSON: {error}")
        if not isinstance(record, dict):
            fail(f"{path}:{number}: record is not an object")
        records.append(record)
    return StoreFile(path, "journal", records, records, original, raw_lines)


def read_structured(path: Path, kind: str) -> StoreFile:
    original = path.read_text(encoding="utf-8")
    try:
        root = json.loads(original)
    except json.JSONDecodeError as error:
        fail(f"{path}: invalid JSON: {error}")
    if kind == "checkpoint":
        if not isinstance(root, dict) or not isinstance(root.get("records"), list):
            fail(f"{path}: unsupported checkpoint shape")
        records = root["records"]
    else:
        if not isinstance(root, list):
            fail(f"{path}: session index is not an array")
        records = root
    if not all(isinstance(record, dict) for record in records):
        fail(f"{path}: record is not an object")
    return StoreFile(path, kind, root, records, original)


def validate_attachment(value: Any, location: str) -> dict[str, Any]:
    if not isinstance(value, dict):
        fail(f"{location}: attachment is not an object")
    if not isinstance(value.get("uri"), str):
        fail(f"{location}: attachment uri is not a string")
    return value


def decode_flattened(values: Any, location: str) -> dict[str, Any]:
    if not isinstance(values, list) or not values or not isinstance(values[0], str):
        fail(f"{location}: malformed flattened attachment")
    tail = values[1:]
    if len(tail) % 2:
        fail(f"{location}: flattened attachment has an unpaired field")
    result: dict[str, Any] = {"uri": values[0]}
    for index in range(0, len(tail), 2):
        key, value = tail[index], tail[index + 1]
        if not isinstance(key, str) or key not in FLATTENED_KEYS or key == "uri":
            fail(f"{location}: unknown flattened attachment field {key!r}")
        if key in result:
            fail(f"{location}: duplicate flattened attachment field {key!r}")
        result[key] = value
    return validate_attachment(result, location)


def canonical_attachments(value: Any, location: str) -> tuple[Any, str]:
    if value is None:
        return None, "empty"
    if isinstance(value, dict):
        if set(value) == {"uri"} and isinstance(value["uri"], list):
            return [decode_flattened(value["uri"], location)], "collapsed"
        return [validate_attachment(value, location)], "legacy-single"
    if not isinstance(value, list):
        fail(f"{location}: attachments are not an array or object")
    if all(isinstance(item, dict) for item in value):
        return [validate_attachment(item, location) for item in value], "canonical"
    result: list[dict[str, Any]] = []
    index = 0
    while index < len(value):
        item = value[index]
        if isinstance(item, dict):
            result.append(validate_attachment(item, f"{location}[{index}]"))
            index += 1
        elif item == "uri" and index + 1 < len(value):
            result.append(decode_flattened(value[index + 1], f"{location}[{index}]"))
            index += 2
        else:
            fail(f"{location}[{index}]: unknown flattened attachment item")
    return result, "flattened"


def migrate_metadata(metadata: dict[str, Any], location: str) -> tuple[bool, str | None]:
    legacy_present = LEGACY_KEY in metadata
    references = metadata.get(REFERENCE_KEY)
    if references is not None and not isinstance(references, dict):
        fail(f"{location}.{REFERENCE_KEY}: owner map is not an object")
    owner = references.get(OWNER_KEY) if isinstance(references, dict) else None
    if owner is not None and not isinstance(owner, dict):
        fail(f"{location}.{REFERENCE_KEY}.{OWNER_KEY}: value is not an object")
    current_present = isinstance(owner, dict) and ATTACHMENTS_KEY in owner
    if not legacy_present and not current_present:
        return False, None
    if legacy_present and current_present:
        fail(f"{location}: both legacy and canonical attachment lanes are present")

    source = owner[ATTACHMENTS_KEY] if current_present else metadata[LEGACY_KEY]
    canonical, shape = canonical_attachments(source, f"{location}.attachments")
    changed = legacy_present or not current_present or canonical != source
    if not changed:
        return False, shape

    next_references = dict(references or {})
    next_owner = dict(owner or {})
    next_owner[ATTACHMENTS_KEY] = canonical
    next_references[OWNER_KEY] = next_owner
    metadata[REFERENCE_KEY] = next_references
    metadata.pop(LEGACY_KEY, None)
    return True, shape


def selected_record(store_file: StoreFile, index: int, session_ids: set[str]) -> bool:
    if store_file.kind != "index" or not session_ids:
        return True
    return store_file.records[index].get("id") in session_ids


def migrate_file(store_file: StoreFile, session_ids: set[str], shapes: Counter[str]) -> None:
    for index, record in enumerate(store_file.records):
        if not selected_record(store_file, index, session_ids):
            continue
        metadata = record.get("metadata")
        if metadata is None:
            continue
        if not isinstance(metadata, dict):
            fail(f"{store_file.path}:record[{index}].metadata is not an object")
        changed, shape = migrate_metadata(
            metadata, f"{store_file.path}:record[{index}].metadata"
        )
        if shape:
            shapes[shape] += 1
        if changed:
            store_file.changed_indices.add(index)


def rendered_journal_lines(store_file: StoreFile) -> list[str]:
    assert store_file.kind == "journal"
    assert store_file.raw_lines is not None
    lines = list(store_file.raw_lines)
    for index in store_file.changed_indices:
        lines[index] = json.dumps(
            store_file.records[index], separators=(",", ":"), ensure_ascii=False
        )
    return lines


def translated_journal_offset(store_file: StoreFile, old_offset: int) -> int:
    """Translate OLD_OFFSET to the same record boundary after journal rewrite."""
    assert store_file.kind == "journal"
    assert store_file.raw_lines is not None
    if store_file.changed_indices and not store_file.original.endswith("\n"):
        fail(f"{store_file.path}: changed journal does not end in a newline")
    rendered = rendered_journal_lines(store_file)
    old_position = 0
    new_position = 0
    if old_offset == 0:
        return 0
    for old_line, new_line in zip(store_file.raw_lines, rendered, strict=True):
        old_position += len(old_line.encode("utf-8")) + 1
        new_position += len(new_line.encode("utf-8")) + 1
        if old_position == old_offset:
            return new_position
        if old_position > old_offset:
            break
    fail(f"{store_file.path}: checkpoint offset {old_offset} is not a record boundary")


def translate_checkpoint_offsets(files: list[StoreFile]) -> int:
    """Update checkpoints paired with rewritten journals; return update count."""
    checkpoints = {
        item.path.name.removesuffix(".checkpoint.json"): item
        for item in files
        if item.kind == "checkpoint"
    }
    updated = 0
    for journal in files:
        if journal.kind != "journal" or not journal.changed_indices:
            continue
        session_id = journal.path.name.removesuffix(".jsonl")
        checkpoint = checkpoints.get(session_id)
        if checkpoint is None:
            continue
        old_offset = checkpoint.root.get("journal-byte-offset")
        if not isinstance(old_offset, int) or isinstance(old_offset, bool):
            fail(f"{checkpoint.path}: journal-byte-offset is not an integer")
        new_offset = translated_journal_offset(journal, old_offset)
        if new_offset != old_offset:
            checkpoint.root["journal-byte-offset"] = new_offset
            checkpoint.structural_changed = True
            updated += 1
    return updated


def changed_file(store_file: StoreFile) -> bool:
    return bool(store_file.changed_indices or store_file.structural_changed)


def render(store_file: StoreFile) -> str:
    if store_file.kind == "journal":
        return "\n".join(rendered_journal_lines(store_file)) + "\n"
    return json.dumps(store_file.root, separators=(",", ":"), ensure_ascii=False) + "\n"


def atomic_write(path: Path, text: str, mode: int) -> None:
    descriptor, temporary = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            stream.write(text)
            stream.flush()
            os.fsync(stream.fileno())
        os.chmod(temporary, stat.S_IMODE(mode))
        os.replace(temporary, path)
    except BaseException:
        try:
            os.unlink(temporary)
        except FileNotFoundError:
            pass
        raise


def session_files(root: Path, session_ids: set[str]) -> list[StoreFile]:
    sessions = root / "sessions"
    if not sessions.is_dir():
        fail(f"missing sessions directory {sessions}")
    files: list[StoreFile] = []
    if session_ids:
        for session_id in sorted(session_ids):
            journal = sessions / f"{session_id}.jsonl"
            if not journal.is_file():
                fail(f"missing journal {journal}")
            files.append(read_jsonl(journal))
            checkpoint = sessions / f"{session_id}.checkpoint.json"
            if checkpoint.exists():
                files.append(read_structured(checkpoint, "checkpoint"))
    else:
        for journal in sorted(sessions.glob("*.jsonl")):
            files.append(read_jsonl(journal))
        for checkpoint in sorted(sessions.glob("*.checkpoint.json")):
            files.append(read_structured(checkpoint, "checkpoint"))
    index = root / "index.json"
    if index.exists():
        files.append(read_structured(index, "index"))
    return files


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--session-root", required=True, type=Path)
    parser.add_argument("--session-id", action="append", default=[])
    parser.add_argument("--apply", action="store_true")
    parser.add_argument("--confirm-emacs-stopped", action="store_true")
    args = parser.parse_args()
    if args.apply and not args.confirm_emacs_stopped:
        fail("apply mode requires --confirm-emacs-stopped")

    root = args.session_root.expanduser().resolve()
    session_ids = set(args.session_id)
    files = session_files(root, session_ids)
    shapes: Counter[str] = Counter()
    for store_file in files:
        migrate_file(store_file, session_ids, shapes)
    checkpoint_offsets_updated = translate_checkpoint_offsets(files)

    changed = [store_file for store_file in files if changed_file(store_file)]
    plan: dict[str, Any] = {
        "mode": "apply" if args.apply else "dry-run",
        "session-root": str(root),
        "session-ids": sorted(session_ids),
        "scanned-files": len(files),
        "changed-files": len(changed),
        "changed-records": sum(len(item.changed_indices) for item in changed),
        "checkpoint-offsets-updated": checkpoint_offsets_updated,
        "attachment-shapes": dict(sorted(shapes.items())),
        "files": [
            {"path": str(item.path), "changed-records": len(item.changed_indices)}
            for item in changed
        ],
    }
    if not args.apply:
        print(json.dumps(plan, indent=2, sort_keys=True))
        return

    stamp = dt.datetime.now(dt.timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    backups: list[str] = []
    for store_file in changed:
        if store_file.path.read_text(encoding="utf-8") != store_file.original:
            fail(f"concurrent modification detected for {store_file.path}")
        backup = store_file.path.with_name(f"{store_file.path.name}.bak.{stamp}")
        if backup.exists():
            fail(f"backup already exists {backup}")
        shutil.copy2(store_file.path, backup)
        backups.append(str(backup))
        atomic_write(store_file.path, render(store_file), store_file.path.stat().st_mode)
    plan["backups"] = backups
    print(json.dumps(plan, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Repair reasoning board messages written with a printed payload plist.

This is an offline, one-off migration.  It is a dry run unless --apply is
passed.  Apply mode creates sibling timestamped backups before atomically
replacing changed journals and checkpoints.

Name every affected board session and any separate producer session containing
its matching durable reasoning activity event with repeated --session-id flags.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import shutil
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Any


BAD_PREFIXES = ("(:type reasoning-delta", "(:content")


def fail(message: str) -> None:
    raise SystemExit(f"repair-reasoning-board-messages: {message}")


@dataclass
class SessionFile:
    path: Path
    kind: str
    records: list[dict[str, Any]]
    raw_lines: list[str] | None = None
    root: dict[str, Any] | None = None
    changed: int = 0
    changed_indices: set[int] | None = None


def read_jsonl(path: Path) -> SessionFile:
    records: list[dict[str, Any]] = []
    raw_lines: list[str] = []
    for number, raw in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        if not raw:
            fail(f"{path}:{number}: blank journal line")
        try:
            record = json.loads(raw)
        except json.JSONDecodeError as error:
            fail(f"{path}:{number}: invalid JSON: {error}")
        if not isinstance(record, dict):
            fail(f"{path}:{number}: record is not an object")
        records.append(record)
        raw_lines.append(raw)
    return SessionFile(path=path, kind="journal", records=records,
                       raw_lines=raw_lines)


def read_checkpoint(path: Path) -> SessionFile:
    try:
        root = json.loads(path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as error:
        fail(f"{path}: invalid JSON: {error}")
    if not isinstance(root, dict) or not isinstance(root.get("records"), list):
        fail(f"{path}: unsupported checkpoint shape")
    records = root["records"]
    if not all(isinstance(record, dict) for record in records):
        fail(f"{path}: checkpoint record is not an object")
    return SessionFile(path=path, kind="checkpoint", records=records, root=root)


def activity_key(record: dict[str, Any]) -> tuple[str, int] | None:
    if record.get("type") != "activity-event":
        return None
    if record.get("event-type") != "reasoning-delta":
        return None
    turn_id = record.get("turn-id")
    sequence = record.get("board-activity-sequence")
    payload = record.get("payload")
    content = payload.get("content") if isinstance(payload, dict) else None
    if not (isinstance(turn_id, str) and isinstance(sequence, int)
            and isinstance(content, str)):
        fail("malformed durable reasoning activity event")
    return turn_id, sequence * 2


def broken_message(record: dict[str, Any]) -> dict[str, Any] | None:
    if record.get("type") != "board-message":
        return None
    message = record.get("message")
    if not isinstance(message, dict):
        return None
    content = message.get("content")
    if (message.get("kind") == "activity"
            and message.get("activity-kind") == "reasoning-delta"
            and isinstance(content, str)
            and content.startswith(BAD_PREFIXES)):
        return message
    return None


def message_key(message: dict[str, Any]) -> tuple[str, int]:
    turn_id = message.get("source-turn-id")
    source_key = message.get("source-activity-key")
    if not (isinstance(turn_id, str) and isinstance(source_key, list)
            and len(source_key) == 3
            and isinstance(source_key[2], int)):
        fail(f"malformed reasoning board message {message.get('id')!r}")
    return turn_id, source_key[2]


def atomic_write(path: Path, text: str) -> None:
    descriptor, temporary = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            stream.write(text)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    except BaseException:
        try:
            os.unlink(temporary)
        except FileNotFoundError:
            pass
        raise


def render_file(session_file: SessionFile) -> str:
    if session_file.kind == "checkpoint":
        return json.dumps(session_file.root, separators=(",", ":")) + "\n"
    assert session_file.raw_lines is not None
    lines = list(session_file.raw_lines)
    for index in session_file.changed_indices or set():
        lines[index] = json.dumps(session_file.records[index], separators=(",", ":"))
    return "\n".join(lines) + "\n"


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--session-root", required=True, type=Path)
    parser.add_argument("--session-id", required=True, action="append")
    parser.add_argument("--apply", action="store_true")
    args = parser.parse_args()

    root = args.session_root.expanduser().resolve()
    sessions_dir = root / "sessions"
    files: list[SessionFile] = []
    for session_id in args.session_id:
        journal = sessions_dir / f"{session_id}.jsonl"
        checkpoint = sessions_dir / f"{session_id}.checkpoint.json"
        if not journal.is_file():
            fail(f"missing journal {journal}")
        files.append(read_jsonl(journal))
        if checkpoint.exists():
            files.append(read_checkpoint(checkpoint))

    activities: dict[tuple[str, int], str] = {}
    for session_file in files:
        for record in session_file.records:
            key = activity_key(record)
            if key is None:
                continue
            content = record["payload"]["content"]
            previous = activities.setdefault(key, content)
            if previous != content:
                fail(f"conflicting reasoning activity for {key!r}")

    repairs: list[dict[str, Any]] = []
    for session_file in files:
        for index, record in enumerate(session_file.records):
            message = broken_message(record)
            if message is None:
                continue
            key = message_key(message)
            content = activities.get(key)
            if content is None:
                fail(f"no durable reasoning activity matches {key!r}")
            message["content"] = content
            session_file.changed += 1
            if session_file.changed_indices is None:
                session_file.changed_indices = set()
            session_file.changed_indices.add(index)
            repairs.append({"file": str(session_file.path),
                            "message-id": message.get("id"),
                            "content-length": len(content)})

    plan = {"mode": "apply" if args.apply else "dry-run",
            "session-root": str(root),
            "session-ids": args.session_id,
            "changed-files": sum(1 for item in files if item.changed),
            "repairs": repairs}
    if not args.apply:
        print(json.dumps(plan, indent=2, sort_keys=True))
        return

    stamp = dt.datetime.now(dt.timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    backups: list[str] = []
    for session_file in files:
        if not session_file.changed:
            continue
        backup = session_file.path.with_name(f"{session_file.path.name}.bak.{stamp}")
        shutil.copy2(session_file.path, backup)
        backups.append(str(backup))
        atomic_write(session_file.path, render_file(session_file))
    plan["backups"] = backups
    print(json.dumps(plan, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()

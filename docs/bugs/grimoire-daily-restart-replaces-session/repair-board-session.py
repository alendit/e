#!/usr/bin/env python3
"""One-off offline repair for a board session missing its root/state records.

The command is a dry run unless --apply is passed.  Apply mode creates sibling
timestamped backups before atomically replacing the journal and derived index.
It intentionally accepts one explicit session id and one explicit board id;
there is no discovery or runtime compatibility path.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import shutil
import tempfile
from pathlib import Path
from typing import Any


def fail(message: str) -> None:
    raise SystemExit(f"repair-board-session: {message}")


def read_json_lines(path: Path) -> tuple[list[dict[str, Any]], list[str]]:
    records: list[dict[str, Any]] = []
    lines: list[str] = []
    for number, raw in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        if not raw:
            continue
        try:
            value = json.loads(raw)
        except json.JSONDecodeError as error:
            fail(f"{path}:{number}: invalid JSON: {error}")
        if not isinstance(value, dict):
            fail(f"{path}:{number}: record is not an object")
        records.append(value)
        lines.append(raw)
    if not records:
        fail(f"{path}: journal is empty")
    return records, lines


def delivery_board_ids(records: list[dict[str, Any]]) -> set[str]:
    board_ids: set[str] = set()
    for record in records:
        message = record.get("message")
        if not isinstance(message, dict):
            continue
        causes = message.get("caused-by-delivery-ids") or []
        if isinstance(causes, dict):
            causes = [causes]
        if not isinstance(causes, list):
            fail("caused-by-delivery-ids is not a list")
        for delivery in causes:
            if isinstance(delivery, list) and delivery and isinstance(delivery[0], str):
                board_ids.add(delivery[0])
            elif isinstance(delivery, dict) and len(delivery) == 1:
                board_id = next(iter(delivery))
                if not isinstance(board_id, str):
                    fail("delivery board id is not a string")
                board_ids.add(board_id)
            else:
                fail(f"unsupported delivery id shape: {delivery!r}")
    return board_ids


def missing_root_id(records: list[dict[str, Any]]) -> str:
    ids: set[str] = set()
    parents: set[str] = set()
    for record in records:
        message = record.get("message")
        for value in (record.get("id"), message.get("id") if isinstance(message, dict) else None):
            if isinstance(value, str):
                ids.add(value)
        for value in (
            record.get("parent-id"),
            message.get("parent-id") if isinstance(message, dict) else None,
        ):
            if isinstance(value, str):
                parents.add(value)
    missing = sorted(parents - ids)
    if len(missing) != 1:
        fail(f"expected one missing root parent id, found {missing!r}")
    return missing[0]


def first_timestamp(records: list[dict[str, Any]]) -> str:
    for record in records:
        timestamp = record.get("timestamp")
        if isinstance(timestamp, str) and timestamp:
            return timestamp
    fail("journal has no timestamped record")


def project_root(records: list[dict[str, Any]]) -> str | None:
    for record in records:
        metadata = record.get("metadata")
        if isinstance(metadata, dict) and isinstance(metadata.get("project-root"), str):
            return metadata["project-root"]
    return None


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


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--session-root", required=True, type=Path)
    parser.add_argument("--session-id", required=True)
    parser.add_argument("--board-id", required=True)
    parser.add_argument("--apply", action="store_true")
    args = parser.parse_args()

    session_root = args.session_root.expanduser().resolve()
    index_path = session_root / "index.json"
    journal_path = session_root / "sessions" / f"{args.session_id}.jsonl"
    if not index_path.is_file() or not journal_path.is_file():
        fail(f"missing index or journal below {session_root}")

    index = json.loads(index_path.read_text(encoding="utf-8"))
    if not isinstance(index, list):
        fail("index.json is not the current array format")
    entries = [entry for entry in index if entry.get("id") == args.session_id]
    if len(entries) != 1:
        fail(f"expected one index entry, found {len(entries)}")
    entry = entries[0]

    records, original_lines = read_json_lines(journal_path)
    foreign = sorted(
        {
            record.get("session-id")
            for record in records
            if record.get("session-id") != args.session_id
        },
        key=repr,
    )
    if foreign:
        fail(f"journal contains foreign session ids: {foreign!r}")
    if any(record.get("type") == "session" for record in records):
        fail("journal already has a session root record")
    if any(record.get("type") == "board-session-state" for record in records):
        fail("journal already has a board-session-state record")

    evidenced_board_ids = delivery_board_ids(records)
    if evidenced_board_ids != {args.board_id}:
        fail(
            f"board evidence {sorted(evidenced_board_ids)!r} does not exactly match "
            f"{args.board_id!r}"
        )

    timestamp = first_timestamp(records)
    root_id = missing_root_id(records)
    principal = f"chat:{args.session_id}"
    metadata: dict[str, Any] = {}
    if root := project_root(records):
        metadata["project-root"] = root
    root_record = {
        "type": "session",
        "session-id": args.session_id,
        "id": root_id,
        "timestamp": timestamp,
        "created-at": timestamp,
        "updated-at": timestamp,
        "metadata": metadata,
    }
    board_record = {
        "type": "board-session-state",
        "session-id": args.session_id,
        "board-state": {"board-id": args.board_id, "principal": principal},
        "board-id": args.board_id,
        "principal": principal,
        "board-output-sequence": 0,
        "board-activity-sequence": 0,
    }

    plan = {
        "mode": "apply" if args.apply else "dry-run",
        "session-id": args.session_id,
        "journal": str(journal_path),
        "index": str(index_path),
        "record-count": len(records),
        "root-id": root_id,
        "created-at": timestamp,
        "board-id": args.board_id,
        "principal": principal,
        "project-root": metadata.get("project-root"),
    }
    if not args.apply:
        print(json.dumps(plan, indent=2, sort_keys=True))
        return

    stamp = dt.datetime.now(dt.timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    journal_backup = journal_path.with_name(f"{journal_path.name}.bak.{stamp}")
    index_backup = index_path.with_name(f"{index_path.name}.bak.{stamp}")
    shutil.copy2(journal_path, journal_backup)
    shutil.copy2(index_path, index_backup)

    journal_text = "\n".join(
        [
            json.dumps(root_record, separators=(",", ":")),
            json.dumps(board_record, separators=(",", ":")),
            *original_lines,
        ]
    ) + "\n"
    entry["created-at"] = timestamp
    entry["board-id"] = args.board_id
    entry["principal"] = principal
    atomic_write(journal_path, journal_text)
    atomic_write(index_path, json.dumps(index, separators=(",", ":")) + "\n")
    plan["journal-backup"] = str(journal_backup)
    plan["index-backup"] = str(index_backup)
    print(json.dumps(plan, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()

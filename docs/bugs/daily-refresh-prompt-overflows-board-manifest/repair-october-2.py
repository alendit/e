#!/usr/bin/env python3
"""Remove the exact abandoned October 2 Daily Board and its three sessions.

Dry run by default. Apply only after the Emacs process that owns the store has
stopped; this historical repair is deliberately separate from the e runtime.
"""

from __future__ import annotations

import argparse
import sqlite3
from pathlib import Path


BOARD = "brd_74784821168f6b17a8c5d10d349ff10b"
COORDINATORS = (
    "20261002T072458-099d1abef261",
    "20261002T074536-ebd50725f00a",
)
OWNER = "ses_7dce1d8a7e5cdcf13656be825d08ddca"
SESSIONS = (*COORDINATORS, OWNER)
PARTICIPANTS = (*COORDINATORS, "ptc_7dce1d8a7e5cdcf13656be825d08ddca")


def require(actual: object, expected: object, label: str) -> None:
    if actual != expected:
        raise ValueError(f"{label} changed: expected {expected!r}, found {actual!r}")


def rows(db: sqlite3.Connection, query: str, parameters: tuple = ()) -> list[tuple]:
    return db.execute(query, parameters).fetchall()


def inspect(db: sqlite3.Connection) -> None:
    require(
        rows(db, "SELECT generation,revision,next_position FROM boards WHERE board_id=?", (BOARD,)),
        [(1, 12, 4)],
        "Board",
    )
    require(
        rows(db, "SELECT participant_id,role,state,publication_pending FROM board_participants WHERE board_id=? ORDER BY participant_id", (BOARD,)),
        [(COORDINATORS[0], "participant", "active", 0),
         (COORDINATORS[1], "participant", "active", 0),
         (PARTICIPANTS[2], "owner", "active", 0)],
        "Board participants",
    )
    require(
        rows(db, "SELECT session_id,participant_id FROM board_session_associations WHERE board_id=? ORDER BY session_id", (BOARD,)),
        [(COORDINATORS[0], COORDINATORS[0]),
         (COORDINATORS[1], COORDINATORS[1]),
         (OWNER, PARTICIPANTS[2])],
        "Board sessions",
    )
    require(
        rows(db, "SELECT position,record_kind FROM board_records WHERE board_id=? ORDER BY position", (BOARD,)),
        [(1, "input"), (2, "activity"), (3, "input"), (4, "output")],
        "Board records",
    )
    require(
        rows(db, "SELECT participant_id,state,fifo_position FROM board_pickups WHERE board_id=?", (BOARD,)),
        [(PARTICIPANTS[2], "consumed", 1)],
        "Board pickups",
    )
    require(
        rows(db, "SELECT run_id FROM board_orchestration_run_index WHERE board_id=?", (BOARD,)),
        [],
        "Board runs",
    )
    require(
        rows(db, "SELECT session_id,journal_position,message_count FROM session_query_state WHERE session_id IN (?,?,?) ORDER BY session_id", SESSIONS),
        [(COORDINATORS[0], 1, 0), (COORDINATORS[1], 1, 0), (OWNER, 236, 38)],
        "Session state",
    )
    require(
        rows(db, "SELECT session_id,COUNT(*) FROM session_records WHERE session_id IN (?,?,?) GROUP BY session_id ORDER BY session_id", SESSIONS),
        [(COORDINATORS[0], 1), (COORDINATORS[1], 1), (OWNER, 236)],
        "Session journals",
    )
    require(rows(db, "PRAGMA foreign_key_check"), [], "Foreign keys")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--database", type=Path,
        default=Path.home() / ".config/emacs/.local/cache/e/store.sqlite3",
    )
    parser.add_argument("--apply", action="store_true")
    args = parser.parse_args()
    database = args.database.expanduser().resolve()
    mode = "rw" if args.apply else "ro"
    db = sqlite3.connect(f"file:{database}?mode={mode}", uri=True, timeout=3)
    try:
        db.execute("PRAGMA foreign_keys=ON")
        db.execute("BEGIN IMMEDIATE" if args.apply else "BEGIN")
        inspect(db)
        if not args.apply:
            print(f"dry run: exact abandoned Board {BOARD} and sessions verified")
            db.rollback()
            return
        db.execute("DELETE FROM boards WHERE board_id=?", (BOARD,))
        for session in SESSIONS:
            db.execute("DELETE FROM session_query_state WHERE session_id=?", (session,))
            db.execute("DELETE FROM session_records WHERE session_id=?", (session,))
            db.execute("DELETE FROM tool_followups WHERE session_id=?", (session,))
            db.execute("DELETE FROM resources WHERE session_id=?", (session,))
        require(rows(db, "SELECT board_id FROM boards WHERE board_id=?", (BOARD,)), [], "Deleted Board")
        require(rows(db, "SELECT session_id FROM session_query_state WHERE session_id IN (?,?,?)", SESSIONS), [], "Deleted sessions")
        require(rows(db, "PRAGMA foreign_key_check"), [], "Foreign keys after delete")
        db.commit()
        print(f"removed Board {BOARD} and {len(SESSIONS)} sessions")
    except BaseException:
        db.rollback()
        raise
    finally:
        db.close()


if __name__ == "__main__":
    main()

import assert from "node:assert/strict";
import test from "node:test";
import { once } from "node:events";
import fs from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { spawn } from "node:child_process";
import { fileURLToPath } from "node:url";
import { compactRecords } from "../lisp/core/e-session-writer.mjs";

const writerPath = fileURLToPath(new URL("../lisp/core/e-session-writer.mjs", import.meta.url));

async function appendThroughWriter(directory, id) {
  const writer = spawn(process.execPath, [writerPath]);
  const output = [];
  writer.stdout.on("data", (chunk) => output.push(chunk));
  writer.stdin.end(JSON.stringify({
    directory, op: "append", "session-id": "session-1", id,
    record: { type: "message", id: "message-1" },
  }) + "\n");
  await once(writer, "exit");
  assert.deepEqual(JSON.parse(Buffer.concat(output).toString("utf8")), { id, ok: true });
}

const manifest = {
  "session-id": "session-1",
  root: { id: "root", "created-at": "fixed", "updated-at": "fixed" },
  "board-message-identities": [{ "record-type": "processing-chain", id: "chain" }],
};

function boardRecord(message, commandId) {
  return {
    type: "board-message",
    "session-id": "session-1",
    message,
    "writer-command-id": commandId,
  };
}

test("writer restart deduplicates a command before its first checkpoint", async (t) => {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), "e-session-writer-"));
  t.after(() => fs.rm(directory, { recursive: true, force: true }));
  await appendThroughWriter(directory, "retry-id");
  await appendThroughWriter(directory, "retry-id");
  const journal = await fs.readFile(path.join(directory, "sessions", "session-1.jsonl"), "utf8");
  assert.equal(journal.trim().split("\n").length, 1);
  assert.equal(await fs.stat(path.join(directory, "sessions", "session-1.checkpoint.json")).then(() => true, () => false), false);
});

test("compaction deduplicates exact typed board-envelope retries", () => {
  const message = {
    id: "chain", "record-type": "processing-chain",
    "root-message-id": "root", "created-at": "fixed",
  };
  const records = [boardRecord(message, "writer-a:1"), boardRecord(message, "writer-b:2")];
  const compacted = compactRecords(records, manifest);
  assert.equal(compacted.filter((record) => record.type === "board-message").length, 1);
  assert.deepEqual(compacted.at(-1).message, message);
});

test("compaction retains the first divergent ordinary board envelope", () => {
  const first = { id: "ordinary", kind: "first" };
  const compacted = compactRecords([
    boardRecord(first, "writer-a:1"),
    boardRecord({ id: "ordinary", kind: "later" }, "writer-b:2"),
  ], {
    ...manifest,
    "board-message-identities": [{ "record-type": "board-message", id: "ordinary" }],
  });
  assert.deepEqual(compacted.at(-1).message, first);
});

test("compaction rejects divergent typed board-envelope duplicates", () => {
  const records = [
    boardRecord({ id: "chain", "record-type": "processing-chain", "root-message-id": "root" }, "writer-a:1"),
    boardRecord({ id: "chain", "record-type": "processing-chain", "root-message-id": "other" }, "writer-b:2"),
  ];
  assert.throws(() => compactRecords(records, manifest), /Conflicting checkpoint board message/);
});

test("compaction starts a fresh board identity domain after clear", () => {
  const records = [
    boardRecord({ id: "shared", kind: "before" }, "writer-a:1"),
    boardRecord({ id: "shared", kind: "before-divergent" }, "writer-a:2"),
    boardRecord({ id: "shared", "record-type": "processing-chain", "root-message-id": "before" }, "writer-a:3"),
    boardRecord({ id: "shared", "record-type": "processing-result", outcome: "before" }, "writer-a:4"),
    { type: "board-messages-cleared", "session-id": "session-1", "writer-command-id": "writer-a:5" },
    boardRecord({ id: "shared", kind: "after" }, "writer-a:6"),
    boardRecord({ id: "shared", kind: "after-divergent" }, "writer-a:7"),
    boardRecord({ id: "shared", "record-type": "processing-chain", "root-message-id": "after" }, "writer-a:8"),
    boardRecord({ id: "shared", "record-type": "processing-result", outcome: "after" }, "writer-a:9"),
  ];
  const clearedManifest = {
    ...manifest,
    "board-message-identities": [
      { "record-type": "board-message", id: "shared" },
      { "record-type": "processing-chain", id: "shared" },
      { "record-type": "processing-result", id: "shared" },
    ],
  };
  const compacted = compactRecords(records, clearedManifest);
  assert.deepEqual(
    compacted.filter((record) => record.type === "board-message").map((record) => record.message),
    [
      { id: "shared", kind: "after" },
      { id: "shared", "record-type": "processing-chain", "root-message-id": "after" },
      { id: "shared", "record-type": "processing-result", outcome: "after" },
    ],
  );
});

test("compaction accepts only the shared board record-type domain", () => {
  const untypedManifest = { ...manifest, "board-message-identities": [{ "record-type": "board-message", id: "message" }] };
  assert.equal(compactRecords([boardRecord({ id: "message" }, "writer-a:1")], untypedManifest).length, 2);
  for (const recordType of ["", 0, false, "unknown"]) {
    assert.throws(
      () => compactRecords([boardRecord({ id: "message", "record-type": recordType }, "writer-a:1")], untypedManifest),
      /Invalid board message record type/,
    );
  }
});

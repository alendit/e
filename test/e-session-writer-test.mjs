import assert from "node:assert/strict";
import test from "node:test";
import { compactRecords } from "../lisp/core/e-session-writer.mjs";

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

test("compaction rejects divergent typed board-envelope duplicates", () => {
  const records = [
    boardRecord({ id: "chain", "record-type": "processing-chain", "root-message-id": "root" }, "writer-a:1"),
    boardRecord({ id: "chain", "record-type": "processing-chain", "root-message-id": "other" }, "writer-b:2"),
  ];
  assert.throws(() => compactRecords(records, manifest), /Conflicting checkpoint board message/);
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

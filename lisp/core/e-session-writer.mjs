#!/usr/bin/env node
// Durable JSONL writer and atomic resume-checkpoint owner.  Emacs supplies the
// semantic retention manifest; this process owns all persistence I/O.

import fs from "node:fs/promises";
import path from "node:path";
import readline from "node:readline";
import { isDeepStrictEqual } from "node:util";
import { fileURLToPath } from "node:url";

const CHECKPOINT_VERSION = 1;
class WriterRequestError extends Error {}

async function sessionsDirectory(directory) {
  const result = path.join(directory, "sessions");
  await fs.mkdir(result, { recursive: true });
  return result;
}

function checkpointPath(directory, sessionId) {
  return path.join(directory, "sessions", `${sessionId}.checkpoint.json`);
}

function parseLines(content, file) {
  if (content && !content.endsWith("\n")) {
    throw new WriterRequestError(`Journal ${file} does not end with a newline`);
  }
  const records = [];
  for (const line of content.split("\n")) {
    if (!line.trim()) continue;
    try {
      records.push(JSON.parse(line));
    } catch (_) {
      throw new WriterRequestError(`Journal ${file} contains malformed JSONL`);
    }
  }
  return records;
}

function commandIds(records) {
  const ids = new Set();
  for (const record of records) {
    if (typeof record?.["writer-command-id"] === "string") {
      ids.add(record["writer-command-id"]);
    }
  }
  return ids;
}

async function readCheckpoint(directory, sessionId) {
  try {
    const checkpoint = JSON.parse(await fs.readFile(checkpointPath(directory, sessionId), "utf8"));
    if (checkpoint.version !== CHECKPOINT_VERSION ||
        checkpoint["session-id"] !== sessionId ||
        !Number.isSafeInteger(checkpoint["journal-byte-offset"]) ||
        checkpoint["journal-byte-offset"] < 0 ||
        !Array.isArray(checkpoint.records)) {
      throw new WriterRequestError(`Invalid session checkpoint ${sessionId}`);
    }
    return checkpoint;
  } catch (error) {
    if (error?.code === "ENOENT") return null;
    throw error;
  }
}

async function journalRecordsAfter(file, offset) {
  const handle = await fs.open(file, "r");
  try {
    const size = (await handle.stat()).size;
    if (offset > size) {
      throw new WriterRequestError(`Checkpoint offset ${offset} exceeds ${file} size ${size}`);
    }
    const buffer = Buffer.alloc(size - offset);
    if (buffer.length) await handle.read(buffer, 0, buffer.length, offset);
    return { size, records: parseLines(buffer.toString("utf8"), file) };
  } finally {
    await handle.close();
  }
}

async function writeAtomicJson(target, value) {
  const temporary = `${target}.${process.pid}.${Date.now()}.tmp`;
  await fs.writeFile(temporary, JSON.stringify(value) + "\n", "utf8");
  await fs.rename(temporary, target);
}

async function knownCommandIds(directory, sessionId) {
  let ids = new Set();
  const dir = await sessionsDirectory(directory);
  const journal = path.join(dir, `${sessionId}.jsonl`);
  try {
    const contents = await journalRecordsAfter(journal, 0);
    ids = commandIds(contents.records);
  } catch (error) {
    if (error?.code !== "ENOENT") throw error;
  }
  return ids;
}

const ENTRY_RECORD_TYPES = new Set([
  "message", "activity-event", "branch-summary", "compaction",
  "provider-anchor", "process-report", "current-branch", "session-info",
  "context-generation", "context-promotion", "context-erasure",
  "context-curation-package", "messages-cleared",
]);

function entryId(record) {
  if (!ENTRY_RECORD_TYPES.has(record?.type)) return null;
  return record.id || record.message?.id || record.report?.id || null;
}

function boardMessageRecordType(recordType) {
  if (recordType === undefined || recordType === null) return "board-message";
  if (recordType === "processing-chain" || recordType === "processing-result") {
    return recordType;
  }
  throw new WriterRequestError(`Invalid board message record type ${JSON.stringify(recordType)}`);
}

function boardMessageIdentity(value) {
  const message = value?.message || value;
  return JSON.stringify([boardMessageRecordType(message?.["record-type"]), message?.id ?? null]);
}

function boardMessageManifestIdentity(identity) {
  const recordType = identity?.["record-type"];
  if (recordType === "board-message") {
    return JSON.stringify([recordType, identity?.id ?? null]);
  }
  return JSON.stringify([boardMessageRecordType(recordType), identity?.id ?? null]);
}

function retainBoardMessage(byIdentity, record) {
  const identity = boardMessageIdentity(record);
  const existing = byIdentity.get(identity);
  if (existing &&
      boardMessageRecordType(record.message?.["record-type"]) !== "board-message" &&
      !isDeepStrictEqual(existing.message, record.message)) {
    throw new WriterRequestError(`Conflicting checkpoint board message ${identity}`);
  }
  if (!existing) byIdentity.set(identity, record);
}

export function compactRecords(records, manifest) {
  const sessionId = manifest["session-id"];
  const root = manifest.root || {};
  const byEntryId = new Map();
  const byBoardMessageIdentity = new Map();
  const displays = new Map();
  for (const record of records) {
    const id = entryId(record);
    if (id) byEntryId.set(id, record);
    if (record.type === "board-message") {
      retainBoardMessage(byBoardMessageIdentity, record);
    } else if (record.type === "board-messages-cleared") {
      byBoardMessageIdentity.clear();
    } else if (record.type === "message-display" && record.id) {
      displays.set(record.id, record);
    }
  }

  const output = [{
    type: "session",
    "session-id": sessionId,
    id: root.id,
    timestamp: root["created-at"],
    "created-at": root["created-at"],
    "updated-at": root["updated-at"],
    metadata: root.metadata ?? null,
    name: root.name ?? null,
    "turn-options": root["turn-options"] ?? null,
    "current-branch": root["current-branch"] ?? null,
    "board-output-sequence": root["board-output-sequence"] || 0,
    "board-activity-sequence": root["board-activity-sequence"] || 0,
  }];

  if (manifest["board-state"]) {
    const state = manifest["board-state"];
    output.push({
      type: "board-session-state", "session-id": sessionId,
      timestamp: root["updated-at"], "board-state": state,
      "board-id": state["board-id"], principal: state.principal,
      "board-output-sequence": root["board-output-sequence"] || 0,
      "board-activity-sequence": root["board-activity-sequence"] || 0,
    });
  }

  for (const identity of manifest["board-message-identities"] || []) {
    const record = byBoardMessageIdentity.get(boardMessageManifestIdentity(identity));
    if (!record) {
      throw new WriterRequestError(
        `Checkpoint board message ${JSON.stringify(identity)} is absent from ${sessionId}`);
    }
    output.push(record);
  }

  let parentId = root.id;
  for (const id of manifest["entry-ids"] || []) {
    const source = byEntryId.get(id);
    if (!source) throw new WriterRequestError(`Checkpoint entry ${id} is absent from ${sessionId}`);
    const record = { ...source, "parent-id": parentId };
    if (source.message) record.message = { ...source.message, "parent-id": parentId };
    if (source.report) record.report = { ...source.report, "parent-id": parentId };
    delete record["writer-command-id"];
    output.push(record);
    parentId = id;
  }
  for (const id of manifest["entry-ids"] || []) {
    if (displays.has(id)) {
      const display = { ...displays.get(id) };
      delete display["writer-command-id"];
      output.push(display);
    }
  }
  return output;
}

async function writeSessionCheckpoint(directory, manifest) {
  const sessionId = manifest?.["session-id"];
  if (typeof sessionId !== "string" || !sessionId) {
    throw new WriterRequestError("Checkpoint manifest needs a session-id");
  }
  const dir = await sessionsDirectory(directory);
  const journal = path.join(dir, `${sessionId}.jsonl`);
  const previous = await readCheckpoint(directory, sessionId);
  const offset = previous?.["journal-byte-offset"] || 0;
  const suffix = await journalRecordsAfter(journal, offset);
  const records = [...(previous?.records || []), ...suffix.records];
  const checkpoint = {
    version: CHECKPOINT_VERSION,
    "session-id": sessionId,
    "journal-byte-offset": suffix.size,
    records: compactRecords(records, manifest),
  };
  await writeAtomicJson(checkpointPath(directory, sessionId), checkpoint);
  return checkpoint;
}

async function ensureInitialCheckpoint(directory, sessionId, record) {
  if (await readCheckpoint(directory, sessionId)) return;
  const dir = await sessionsDirectory(directory);
  const journal = path.join(dir, `${sessionId}.jsonl`);
  const size = (await fs.stat(journal)).size;
  await writeAtomicJson(checkpointPath(directory, sessionId), {
    version: CHECKPOINT_VERSION,
    "session-id": sessionId,
    "journal-byte-offset": size,
    records: [record],
  });
}

export function updateIndexEntry(entry, record, file) {
  const timestamp = record.timestamp || record["updated-at"] || entry["updated-at"];
  entry["updated-at"] = timestamp || entry["updated-at"];
  entry.file = file;
  if (record.type === "session") {
    entry.id = record["session-id"];
    entry["created-at"] = record["created-at"] || record.timestamp;
    entry.metadata = record.metadata || entry.metadata || null;
    entry.name = record.name || entry.name || null;
  } else if (record.type === "session-info") {
    if (record.metadata) entry.metadata = record.metadata;
    if (record.name) entry.name = record.name;
  } else if (record.type === "message") {
    const message = record.message || {};
    entry["message-count"] = (entry["message-count"] || 0) + 1;
    entry["last-message-at"] = message["created-at"] || timestamp;
    if (!entry.summary && message.role === "user" && typeof message.content === "string") entry.summary = message.content;
    if (message.role === "assistant") entry["latest-assistant-marker"] = message.id || message["created-at"];
  } else if (record.type === "messages-cleared") {
    entry["message-count"] = 0;
    entry.summary = null;
    entry["last-message-at"] = null;
    entry["latest-assistant-marker"] = null;
  } else if (record.type === "board-session-state") {
    entry["board-id"] = record["board-id"] || record["board-state"]?.["board-id"];
    entry.principal = record.principal || record["board-state"]?.principal;
    entry["board-state"] = record["board-state"] || entry["board-state"];
  }
}

function titleFor(entry) {
  if (entry.name) return entry.name;
  if (entry.summary) return entry.summary.length > 25 ? `${entry.summary.slice(0, 25)}...` : entry.summary;
  return `Untitled ${entry["created-at"] || entry.id}`;
}

export async function rebuildIndex(directory) {
  const dir = await sessionsDirectory(directory);
  const entries = [];
  for (const name of await fs.readdir(dir)) {
    if (!name.endsWith(".jsonl")) continue;
    const sessionId = path.basename(name, ".jsonl");
    const file = path.join(dir, name);
    const checkpoint = await readCheckpoint(directory, sessionId);
    let records;
    if (checkpoint) {
      const suffix = await journalRecordsAfter(file, checkpoint["journal-byte-offset"]);
      records = [...checkpoint.records, ...suffix.records];
    } else {
      const handle = await fs.open(file, "r");
      try {
        const buffer = Buffer.alloc(Math.min(65536, (await handle.stat()).size));
        await handle.read(buffer, 0, buffer.length, 0);
        const firstLineEnd = buffer.indexOf(0x0a);
        if (firstLineEnd < 0) {
          throw new WriterRequestError(`Journal ${file} has no complete first record`);
        }
        records = parseLines(buffer.subarray(0, firstLineEnd + 1).toString("utf8"), file);
      } finally { await handle.close(); }
    }
    const entry = { id: sessionId, "message-count": 0, loaded: false, file };
    for (const record of records) updateIndexEntry(entry, record, file);
    entry.title = titleFor(entry);
    entries.push(entry);
  }
  entries.sort((a, b) => String(b["last-message-at"] || b["created-at"] || "").localeCompare(String(a["last-message-at"] || a["created-at"] || "")));
  await writeAtomicJson(path.join(directory, "index.json"), entries);
}

async function handle(request) {
  const directory = request.directory;
  if (typeof directory !== "string" || !directory || typeof request.op !== "string" || !request.op) {
    throw new WriterRequestError("Writer request needs string directory and op");
  }
  if (request.op === "append") {
    const sessionId = request["session-id"];
    const knownIds = await knownCommandIds(directory, sessionId);
    if (!knownIds.has(request.id)) {
      const dir = await sessionsDirectory(directory);
      const record = { ...request.record, "writer-command-id": request.id };
      await fs.appendFile(path.join(dir, `${sessionId}.jsonl`), JSON.stringify(record) + "\n", "utf8");
      if (record.type === "session") await ensureInitialCheckpoint(directory, sessionId, record);
    }
  } else if (request.op === "checkpoint") {
    if (!Array.isArray(request.sessions) || request.sessions.length !== 1) {
      throw new WriterRequestError("Checkpoint request needs exactly one session manifest");
    }
    await writeSessionCheckpoint(directory, request.sessions[0]);
  } else if (request.op === "reindex") {
    await rebuildIndex(directory);
  } else {
    throw new WriterRequestError(`Unsupported writer operation ${request.op}`);
  }
}

if (process.argv[1] && fileURLToPath(import.meta.url) === path.resolve(process.argv[1])) {
  const input = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });
  let chain = Promise.resolve();
  input.on("line", (line) => {
    if (!line.trim()) return;
    chain = chain.then(async () => {
      let request;
      try {
        request = JSON.parse(line);
        const result = await handle(request);
        process.stdout.write(JSON.stringify({ id: request.id, ok: true, result }) + "\n");
      } catch (error) {
        process.stdout.write(JSON.stringify({
          id: request?.id ?? null, ok: false,
          retryable: !(error instanceof WriterRequestError || error instanceof SyntaxError),
          error: error?.message || String(error),
        }) + "\n");
      }
    });
  });
}

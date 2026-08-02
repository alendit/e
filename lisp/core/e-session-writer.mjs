#!/usr/bin/env node
// Durable JSONL writer for e session persistence.  This process intentionally
// owns filesystem I/O and catalog rebuilding; Emacs only owns the outbox.

import fs from "node:fs/promises";
import path from "node:path";
import readline from "node:readline";
import { fileURLToPath } from "node:url";

const knownCommandsByDirectory = new Map();

async function sessionsDirectory(directory) {
  const result = path.join(directory, "sessions");
  await fs.mkdir(result, { recursive: true });
  return result;
}

async function knownCommands(directory) {
  if (knownCommandsByDirectory.has(directory)) return knownCommandsByDirectory.get(directory);
  const known = new Set();
  const dir = await sessionsDirectory(directory);
  for (const file of await fs.readdir(dir)) {
    if (!file.endsWith(".jsonl")) continue;
    const content = await fs.readFile(path.join(dir, file), "utf8").catch(() => "");
    for (const line of content.split("\n")) {
      try {
        const id = JSON.parse(line)["writer-command-id"];
        if (typeof id === "string" && id) known.add(id);
      } catch (_) {}
    }
  }
  knownCommandsByDirectory.set(directory, known);
  return known;
}

export function updateCatalogEntry(entry, record, file) {
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
    if (!entry.summary && message.role === "user" && typeof message.content === "string") {
      entry.summary = message.content;
    }
    if (message.role === "assistant") entry["latest-assistant-marker"] = message.id || message["created-at"];
  } else if (record.type === "messages-cleared") {
    entry["message-count"] = 0;
    entry.summary = null;
    entry["last-message-at"] = null;
    entry["latest-assistant-marker"] = null;
  } else if (record.type === "board-session-state") {
    entry.state = record.state;
    entry["access-record"] = record["access-record"];
    entry["board-output-sequence"] = record["board-output-sequence"];
    entry["board-activity-sequence"] = record["board-activity-sequence"];
  }
}

function titleFor(entry) {
  if (entry.name) return entry.name;
  if (entry.summary) return entry.summary.length > 25 ? `${entry.summary.slice(0, 25)}...` : entry.summary;
  return `Untitled ${entry["created-at"] || entry.id}`;
}

export async function rebuildCatalog(directory) {
  const dir = await sessionsDirectory(directory);
  const entries = [];
  for (const name of await fs.readdir(dir)) {
    if (!name.endsWith(".jsonl")) continue;
    const file = path.join(dir, name);
    const entry = { id: path.basename(name, ".jsonl"), "message-count": 0, loaded: false, file };
    const content = await fs.readFile(file, "utf8").catch(() => "");
    for (const line of content.split("\n")) {
      try { updateCatalogEntry(entry, JSON.parse(line), file); } catch (_) {}
    }
    entry.title = titleFor(entry);
    entries.push(entry);
  }
  entries.sort((a, b) => String(b["last-message-at"] || b["created-at"] || "").localeCompare(String(a["last-message-at"] || a["created-at"] || "")));
  const target = path.join(directory, "index.json");
  const temporary = `${target}.${process.pid}.tmp`;
  await fs.writeFile(temporary, JSON.stringify(entries) + "\n", "utf8");
  await fs.rename(temporary, target);
}

async function handle(request) {
  const directory = request.directory;
  if (!directory || !request.op) throw new Error("Writer request needs directory and op");
  if (request.op === "append") {
    const known = await knownCommands(directory);
    if (!known.has(request.id)) {
      const dir = await sessionsDirectory(directory);
      const record = { ...request.record, "writer-command-id": request.id };
      await fs.appendFile(path.join(dir, `${request["session-id"]}.jsonl`), JSON.stringify(record) + "\n", "utf8");
      known.add(request.id);
    }
  } else if (request.op === "checkpoint") {
    await rebuildCatalog(directory);
  } else if (request.op === "catalog-page") {
    const target = path.join(directory, "index.json");
    const entries = JSON.parse(await fs.readFile(target, "utf8").catch(() => "[]"));
    const rows = entries
      .map((entry) => ({
        "session-id": entry.id,
        state: entry.state,
        "access-record": entry["access-record"],
        "board-output-sequence": entry["board-output-sequence"],
        "board-activity-sequence": entry["board-activity-sequence"],
      }))
      .sort((left, right) => String(left["session-id"]).localeCompare(String(right["session-id"])));
    if (request["catalog-operation"] === "read") {
      return rows.find((row) => row["session-id"] === request["session-id"]) ?? null;
    }
    const after = request.after == null ? null : String(request.after);
    const limit = Math.max(1, Math.min(Number(request.limit) || 32, 32));
    const start = after == null ? 0 : rows.findIndex((row) => String(row["session-id"]) > after);
    const offset = start < 0 ? rows.length : start;
    const page = rows.slice(offset, offset + limit);
    const more = offset + page.length < rows.length;
    return { sessions: page, "next-after": more ? page.at(-1)["session-id"] : null };
  } else {
    throw new Error(`Unsupported writer operation ${request.op}`);
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
        process.stdout.write(JSON.stringify({ id: request?.id ?? null, ok: false, error: error?.message || String(error) }) + "\n");
      }
    });
  });
}

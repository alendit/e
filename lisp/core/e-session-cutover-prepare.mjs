#!/usr/bin/env node
// One-shot offline preparation for the board-only session schema.  This command
// must run only after the caller has quiesced every process that can write the
// target store.  It validates all input before copying or replacing the store.

import fs from "node:fs/promises";
import path from "node:path";
import { rebuildCatalog } from "./e-session-writer.mjs";

const SCHEMA_VERSION = 1;

function fail(message) {
  throw new Error(message);
}

function parseArguments(argv) {
  const result = {};
  for (let index = 0; index < argv.length; index += 2) {
    const flag = argv[index];
    const value = argv[index + 1];
    if (!value || !["--store", "--manifest", "--snapshot"].includes(flag)) {
      fail("Usage: e-session-cutover-prepare.mjs --store DIR --manifest FILE --snapshot DIR");
    }
    result[flag.slice(2)] = value;
  }
  if (!result.store || !result.manifest || !result.snapshot) {
    fail("Preparation requires --store, --manifest, and --snapshot");
  }
  return result;
}

async function pathExists(target) {
  return fs.lstat(target).then(() => true, (error) => {
    if (error.code === "ENOENT") return false;
    throw error;
  });
}

function requireString(value, description) {
  if (typeof value !== "string" || value.length === 0) fail(`${description} must be a non-empty string`);
  return value;
}

function requirePrincipalList(value, description) {
  if (!Array.isArray(value) || value.some((principal) => typeof principal !== "string" || !principal)) {
    fail(`${description} must be an array of non-empty principal strings`);
  }
  if (new Set(value).size !== value.length) fail(`${description} contains duplicate principals`);
  return [...value];
}

async function readJson(file, description) {
  let value;
  try {
    value = JSON.parse(await fs.readFile(file, "utf8"));
  } catch (error) {
    fail(`${description} is not valid JSON: ${error.message}`);
  }
  return value;
}

function validateManifest(manifest) {
  if (!manifest || typeof manifest !== "object" || Array.isArray(manifest)) fail("Manifest must be an object");
  if (manifest["schema-version"] !== SCHEMA_VERSION) fail(`Manifest schema-version must be ${SCHEMA_VERSION}`);
  const storeId = requireString(manifest["session-store-id"], "Manifest session-store-id");
  if (!Array.isArray(manifest.sessions)) fail("Manifest sessions must be an array");
  const policies = new Map();
  for (const policy of manifest.sessions) {
    if (!policy || typeof policy !== "object" || Array.isArray(policy)) fail("Every session policy must be an object");
    const sessionId = requireString(policy["session-id"], "Session policy session-id");
    if (policies.has(sessionId)) fail(`Manifest contains duplicate policy for ${sessionId}`);
    policies.set(sessionId, {
      controller: requireString(policy.controller, `Controller for ${sessionId}`),
      discover: requirePrincipalList(policy["discover-principals"], `Discover principals for ${sessionId}`),
      resume: requirePrincipalList(policy["resume-principals"], `Resume principals for ${sessionId}`),
    });
  }
  return { storeId, policies };
}

async function readJournal(file, expectedSessionId) {
  const records = [];
  const content = await fs.readFile(file, "utf8");
  for (const [offset, text] of content.split("\n").entries()) {
    if (!text.trim()) continue;
    let record;
    try {
      record = JSON.parse(text);
    } catch (error) {
      fail(`${file}:${offset + 1} is not valid JSON: ${error.message}`);
    }
    if (!record || typeof record !== "object" || Array.isArray(record)) fail(`${file}:${offset + 1} is not a record object`);
    records.push(record);
  }
  const roots = records.filter((record) => record.type === "session");
  if (roots.length !== 1 || roots[0]["session-id"] !== expectedSessionId) {
    fail(`${file} must contain exactly one matching session root`);
  }
  if (records.some((record) => record.type === "board-session-state")) {
    fail(`${file} already contains board-session-state`);
  }
  let outputSequence = 0;
  let activitySequence = 0;
  for (const record of records) {
    const output = record.message?.["board-output-sequence"];
    const activity = record["board-activity-sequence"];
    if (output !== undefined) {
      if (!Number.isSafeInteger(output) || output < 0) fail(`${file} has an invalid board-output-sequence`);
      outputSequence = Math.max(outputSequence, output);
    }
    if (activity !== undefined) {
      if (!Number.isSafeInteger(activity) || activity < 0) fail(`${file} has an invalid board-activity-sequence`);
      activitySequence = Math.max(activitySequence, activity);
    }
  }
  return { outputSequence, activitySequence };
}

async function inspectStore(store, policies) {
  const sessionsDirectory = path.join(store, "sessions");
  const names = (await fs.readdir(sessionsDirectory)).filter((name) => name.endsWith(".jsonl")).sort();
  const sessions = [];
  for (const name of names) {
    const sessionId = path.basename(name, ".jsonl");
    if (!policies.has(sessionId)) fail(`Manifest has no policy for stored session ${sessionId}`);
    sessions.push({
      sessionId,
      name,
      ...(await readJournal(path.join(sessionsDirectory, name), sessionId)),
    });
  }
  for (const sessionId of policies.keys()) {
    if (!sessions.some((session) => session.sessionId === sessionId)) {
      fail(`Manifest policy ${sessionId} has no stored session`);
    }
  }
  return sessions;
}

function stateRecord(session, policy, storeId, timestamp) {
  return {
    type: "board-session-state",
    "schema-version": SCHEMA_VERSION,
    "session-store-id": storeId,
    "session-id": session.sessionId,
    timestamp,
    state: "dormant",
    "access-record": {
      controller: policy.controller,
      version: 0,
      "discover-principals": policy.discover,
      "resume-principals": policy.resume,
    },
    "board-output-sequence": session.outputSequence,
    "board-activity-sequence": session.activitySequence,
  };
}

async function prepareCopy(work, sessions, policies, storeId, timestamp) {
  for (const session of sessions) {
    const record = stateRecord(session, policies.get(session.sessionId), storeId, timestamp);
    await fs.appendFile(path.join(work, "sessions", session.name), `${JSON.stringify(record)}\n`, "utf8");
  }
  await rebuildCatalog(work);
  const catalog = await readJson(path.join(work, "index.json"), "Prepared catalog");
  if (!Array.isArray(catalog) || catalog.length !== sessions.length) fail("Prepared catalog does not cover every session");
  for (const row of catalog) {
    if (row.state !== "dormant" || !row["access-record"] ||
        !Number.isSafeInteger(row["board-output-sequence"]) ||
        !Number.isSafeInteger(row["board-activity-sequence"])) {
      fail(`Prepared catalog row ${row.id ?? "<missing>"} is not current`);
    }
  }
}

async function main() {
  const arguments_ = parseArguments(process.argv.slice(2));
  const store = path.resolve(arguments_.store);
  const manifestFile = path.resolve(arguments_.manifest);
  const snapshot = path.resolve(arguments_.snapshot);
  const parent = path.dirname(store);
  const base = path.basename(store);
  const lock = path.join(parent, `.${base}.board-cutover-preparation.lock`);
  const work = path.join(parent, `.${base}.board-cutover-work-${process.pid}-${Date.now()}`);
  const hold = path.join(parent, `.${base}.board-cutover-old-${process.pid}-${Date.now()}`);
  let lockHandle;
  let lockOwned = false;
  let storeHeld = false;
  try {
    if (!(await pathExists(store))) fail(`Session store does not exist: ${store}`);
    if (await pathExists(snapshot)) fail(`Snapshot target already exists: ${snapshot}`);
    const relativeSnapshot = path.relative(store, snapshot);
    if (relativeSnapshot === "" || (!relativeSnapshot.startsWith("..") && !path.isAbsolute(relativeSnapshot))) {
      fail("Snapshot must be outside the session store");
    }
    lockHandle = await fs.open(lock, "wx").catch((error) => {
      if (error.code === "EEXIST") fail(`Preparation lock already exists: ${lock}`);
      throw error;
    });
    lockOwned = true;
    const { storeId, policies } = validateManifest(await readJson(manifestFile, "Manifest"));
    const sessions = await inspectStore(store, policies);
    const timestamp = new Date().toISOString();
    await fs.cp(store, snapshot, { recursive: true, errorOnExist: true, force: false });
    await fs.cp(store, work, { recursive: true, errorOnExist: true, force: false });
    await prepareCopy(work, sessions, policies, storeId, timestamp);
    await fs.rename(store, hold);
    storeHeld = true;
    try {
      await fs.rename(work, store);
      storeHeld = false;
    } catch (error) {
      await fs.rename(hold, store);
      storeHeld = false;
      throw error;
    }
    await fs.rm(hold, { recursive: true, force: true });
    process.stdout.write(`${JSON.stringify({ ok: true, store, snapshot, sessions: sessions.length })}\n`);
  } finally {
    if (storeHeld && !(await pathExists(store)) && await pathExists(hold)) await fs.rename(hold, store);
    if (await pathExists(work)) await fs.rm(work, { recursive: true, force: true });
    if (lockHandle) await lockHandle.close();
    if (lockOwned && await pathExists(lock)) await fs.rm(lock, { force: true });
  }
}

main().catch((error) => {
  process.stderr.write(`${error.message}\n`);
  process.exitCode = 1;
});

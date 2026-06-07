#!/usr/bin/env node

import { spawn } from "node:child_process";
import readline from "node:readline";

const sessions = new Map();

function respond(id, payload) {
  process.stdout.write(`${JSON.stringify({ id, ...payload })}\n`);
}

function ok(id, result, diagnostics = {}) {
  respond(id, { ok: true, result, diagnostics });
}

function fail(id, error, diagnostics = {}) {
  respond(id, { ok: false, error: String(error && error.message ? error.message : error), diagnostics });
}

function frame(payload) {
  const body = Buffer.from(JSON.stringify(payload), "utf8");
  return Buffer.concat([
    Buffer.from(`Content-Length: ${body.length}\r\n\r\n`, "utf8"),
    body
  ]);
}

class McpSession {
  constructor(config) {
    this.config = config;
    this.process = null;
    this.buffer = Buffer.alloc(0);
    this.nextId = 0;
    this.pending = new Map();
    this.stderr = "";
    this.initialized = false;
    this.stale = false;
  }

  diagnostics() {
    return {
      server: this.config.id,
      stderr: this.stderr,
      stale: this.stale
    };
  }

  ensureProcess() {
    if (
      this.process &&
      !this.process.killed &&
      this.process.exitCode === null &&
      this.process.signalCode === null
    ) {
      return;
    }
    const command = Array.from(this.config.command || []);
    if (command.length === 0) throw new Error(`MCP server ${this.config.id} has no command`);
    const [program, ...args] = command;
    const extraEnv = {};
    for (const entry of Array.from(this.config.env || [])) {
      if (entry && typeof entry.name === "string") {
        extraEnv[entry.name] = String(entry.value ?? "");
      }
    }
    this.process = spawn(program, args, {
      stdio: ["pipe", "pipe", "pipe"],
      env: { ...process.env, ...extraEnv }
    });
    this.process.stdout.on("data", (chunk) => this.onData(chunk));
    this.process.stderr.on("data", (chunk) => {
      this.stderr += chunk.toString("utf8");
    });
    this.process.on("exit", (code, signal) => {
      const error = new Error(`MCP server ${this.config.id} exited with ${signal || code}`);
      for (const { reject, timer } of this.pending.values()) {
        clearTimeout(timer);
        reject(error);
      }
      this.pending.clear();
      this.initialized = false;
      this.process = null;
      this.buffer = Buffer.alloc(0);
    });
  }

  onData(chunk) {
    this.buffer = Buffer.concat([this.buffer, chunk]);
    while (true) {
      const headerEnd = this.buffer.indexOf("\r\n\r\n");
      if (headerEnd === -1) return;
      const header = this.buffer.slice(0, headerEnd).toString("utf8");
      const match = header.match(/Content-Length:\s*(\d+)/i);
      if (!match) throw new Error(`MCP server ${this.config.id} sent a frame without Content-Length`);
      const length = Number(match[1]);
      const start = headerEnd + 4;
      const end = start + length;
      if (this.buffer.length < end) return;
      const body = this.buffer.slice(start, end).toString("utf8");
      this.buffer = this.buffer.slice(end);
      this.onMessage(JSON.parse(body));
    }
  }

  onMessage(message) {
    if (message.method === "notifications/tools/list_changed") {
      this.stale = true;
      return;
    }
    if (Object.prototype.hasOwnProperty.call(message, "id")) {
      const pending = this.pending.get(message.id);
      if (!pending) return;
      this.pending.delete(message.id);
      clearTimeout(pending.timer);
      if (message.error) {
        pending.reject(new Error(message.error.message || JSON.stringify(message.error)));
      } else {
        pending.resolve(message.result);
      }
    }
  }

  request(method, params = {}) {
    this.ensureProcess();
    const id = ++this.nextId;
    const timeout = Number(this.config.timeout || 10) * 1000;
    const payload = { jsonrpc: "2.0", id, method, params };
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        this.pending.delete(id);
        reject(new Error(`MCP server ${this.config.id} timed out during ${method}`));
      }, timeout);
      this.pending.set(id, { resolve, reject, timer });
      this.process.stdin.write(frame(payload));
    });
  }

  notify(method, params = {}) {
    this.ensureProcess();
    this.process.stdin.write(frame({ jsonrpc: "2.0", method, params }));
  }

  async initialize() {
    if (this.initialized) return;
    await this.request("initialize", {
      protocolVersion: "2024-11-05",
      capabilities: {},
      clientInfo: { name: "e-mcp-helper", version: "0.1.0" }
    });
    this.notify("notifications/initialized", {});
    this.initialized = true;
  }

  async listTools() {
    await this.initialize();
    const result = await this.request("tools/list", {});
    this.stale = false;
    return Array.from(result.tools || []);
  }

  async callTool(name, args) {
    await this.initialize();
    return await this.request("tools/call", { name, arguments: args || {} });
  }

  async refresh() {
    const tools = await this.listTools();
    this.stale = false;
    return tools;
  }

  stop() {
    if (this.process && !this.process.killed) {
      this.process.kill();
    }
  }
}

function ensureSessions(configs) {
  for (const config of Array.from(configs || [])) {
    if (!config || typeof config.id !== "string") continue;
    const existing = sessions.get(config.id);
    if (!existing) {
      sessions.set(config.id, new McpSession(config));
    } else {
      existing.config = config;
    }
  }
}

function sessionFor(id) {
  const session = sessions.get(id);
  if (!session) throw new Error(`Unknown MCP server: ${id}`);
  return session;
}

async function handle(request) {
  ensureSessions(request.servers);
  if (request.op === "list-tools") {
    const tools = [];
    const diagnostics = {};
    for (const config of Array.from(request.servers || [])) {
      const session = sessionFor(config.id);
      const listed = await session.listTools();
      tools.push(...listed.map((tool) => ({ serverId: config.id, ...tool })));
      diagnostics[config.id] = session.diagnostics();
    }
    return { result: { tools }, diagnostics };
  }
  if (request.op === "call-tool") {
    const session = sessionFor(request.server);
    const result = await session.callTool(request.tool, request.arguments);
    return { result, diagnostics: session.diagnostics() };
  }
  if (request.op === "refresh") {
    const diagnostics = {};
    const tools = [];
    for (const config of Array.from(request.servers || [])) {
      const session = sessionFor(config.id);
      const listed = await session.refresh();
      tools.push(...listed.map((tool) => ({ serverId: config.id, ...tool })));
      diagnostics[config.id] = session.diagnostics();
    }
    return { result: { refreshed: true, tools }, diagnostics };
  }
  throw new Error(`Unsupported MCP helper operation: ${request.op}`);
}

const rl = readline.createInterface({
  input: process.stdin,
  crlfDelay: Infinity
});

rl.on("line", async (line) => {
  if (!line.trim()) return;
  let request;
  try {
    request = JSON.parse(line);
    const { result, diagnostics } = await handle(request);
    ok(request.id, result, diagnostics);
  } catch (error) {
    fail(request && request.id, error, { helperStderr: String(error && error.stack ? error.stack : error) });
  }
});

process.on("exit", () => {
  for (const session of sessions.values()) {
    session.stop();
  }
});

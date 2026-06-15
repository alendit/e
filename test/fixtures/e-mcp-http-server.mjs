#!/usr/bin/env node

// Minimal stateful Streamable-HTTP MCP fixture server.
//
// It assigns an Mcp-Session-Id on `initialize` and REQUIRES that header on
// every later request.  This exercises the Emacs HTTP transport's session-id
// capture: a client that fails to echo the header back gets 400 and cannot
// list or call tools.  The chosen port is printed to stdout as a single line
// so the test can read it.

import http from "node:http";
import crypto from "node:crypto";

const tools = [
  {
    name: "echo",
    description: "Echo text.",
    inputSchema: {
      type: "object",
      properties: { text: { type: "string" } }
    }
  }
];

let sessionId = null;

function send(res, status, payload) {
  const body = payload == null ? "" : JSON.stringify(payload);
  const headers = { "Content-Type": "application/json" };
  if (payload && payload.__sessionId) {
    headers["Mcp-Session-Id"] = payload.__sessionId;
    delete payload.__sessionId;
  }
  res.writeHead(status, headers);
  res.end(body);
}

const server = http.createServer((req, res) => {
  let raw = "";
  req.on("data", (chunk) => {
    raw += chunk;
  });
  req.on("end", () => {
    const message = raw ? JSON.parse(raw) : {};
    const method = message.method;
    const id = message.id;
    const incomingSession = req.headers["mcp-session-id"];

    if (method === "initialize") {
      sessionId = crypto.randomUUID();
      send(res, 200, {
        jsonrpc: "2.0",
        id,
        __sessionId: sessionId,
        result: {
          protocolVersion: "2024-11-05",
          capabilities: { tools: { listChanged: true } },
          serverInfo: { name: "e-mcp-http-fixture", version: "0.0.0" }
        }
      });
      return;
    }

    // notifications/initialized has no id and expects no JSON-RPC result.
    if (method === "notifications/initialized") {
      send(res, 202, null);
      return;
    }

    // Every other request must carry the session header from initialize.
    if (!sessionId || incomingSession !== sessionId) {
      send(res, 400, {
        jsonrpc: "2.0",
        id,
        error: { code: -32000, message: "Missing or stale Mcp-Session-Id" }
      });
      return;
    }

    if (method === "tools/list") {
      send(res, 200, { jsonrpc: "2.0", id, result: { tools } });
      return;
    }

    if (method === "tools/call") {
      const { name, arguments: args = {} } = message.params || {};
      if (name === "echo") {
        send(res, 200, {
          jsonrpc: "2.0",
          id,
          result: {
            content: [{ type: "text", text: String(args.text || "") }],
            isError: false
          }
        });
        return;
      }
      send(res, 200, {
        jsonrpc: "2.0",
        id,
        error: { code: -32602, message: `Unknown tool: ${name}` }
      });
      return;
    }

    send(res, 200, { jsonrpc: "2.0", id, result: {} });
  });
});

server.listen(0, "127.0.0.1", () => {
  const { port } = server.address();
  process.stdout.write(`${port}\n`);
});

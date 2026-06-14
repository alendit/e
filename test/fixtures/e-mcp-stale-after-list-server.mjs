#!/usr/bin/env node

let buffer = Buffer.alloc(0);

function writeMessage(payload) {
  const body = Buffer.from(JSON.stringify(payload), "utf8");
  process.stdout.write(`Content-Length: ${body.length}\r\n\r\n`);
  process.stdout.write(body);
}

function respond(id, result) {
  writeMessage({ jsonrpc: "2.0", id, result });
}

function readMessages(chunk, onMessage) {
  buffer = Buffer.concat([buffer, chunk]);
  while (true) {
    const headerEnd = buffer.indexOf("\r\n\r\n");
    if (headerEnd === -1) return;
    const header = buffer.slice(0, headerEnd).toString("utf8");
    const match = header.match(/Content-Length:\s*(\d+)/i);
    if (!match) throw new Error("Missing Content-Length");
    const length = Number(match[1]);
    const start = headerEnd + 4;
    const end = start + length;
    if (buffer.length < end) return;
    const body = buffer.slice(start, end).toString("utf8");
    buffer = buffer.slice(end);
    onMessage(JSON.parse(body));
  }
}

const tools = [
  {
    name: "echo",
    description: "Echo text.",
    inputSchema: { type: "object" }
  }
];

process.stdin.on("data", (chunk) => {
  readMessages(chunk, (message) => {
    if (message.method === "initialize") {
      respond(message.id, {
        protocolVersion: "2024-11-05",
        capabilities: { tools: { listChanged: true } },
        serverInfo: { name: "e-mcp-stale-after-list", version: "0.0.0" }
      });
      return;
    }
    if (message.method === "tools/list") {
      respond(message.id, { tools });
      setTimeout(() => {
        writeMessage({
          jsonrpc: "2.0",
          method: "notifications/tools/list_changed",
          params: { sequence: 1 }
        });
      }, 0);
      return;
    }
    if (message.method === "tools/call") {
      respond(message.id, {
        content: [{ type: "text", text: "ok" }],
        isError: false
      });
    }
  });
});

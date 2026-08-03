import fs from "node:fs/promises";
import path from "node:path";

const target = process.argv[2];
if (!target) throw new Error("missing task queue snapshot path");

const chunks = [];
for await (const chunk of process.stdin) chunks.push(chunk);
const content = Buffer.concat(chunks);
const temporary = `${target}.tmp-${process.pid}`;

await fs.mkdir(path.dirname(target), { recursive: true });
await fs.writeFile(temporary, content);
await fs.rename(temporary, target);

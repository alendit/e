#!/usr/bin/env node
// Basic Playwright helper for e web browser tools.

import fs from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import readline from "node:readline";

let chromiumModule = null;
let browser = null;
let context = null;
const pages = new Map();

async function loadPlaywright() {
  if (!chromiumModule) {
    const playwright = await import("playwright");
    chromiumModule = playwright.chromium;
  }
  return chromiumModule;
}

async function ensureContext() {
  if (!browser) {
    const chromium = await loadPlaywright();
    browser = await chromium.launch({ headless: true });
  }
  if (!context) {
    context = await browser.newContext();
  }
  return context;
}

async function pageFor(session = "default") {
  const existing = pages.get(session);
  if (existing && !existing.isClosed()) {
    return existing;
  }
  const ctx = await ensureContext();
  const page = await ctx.newPage();
  pages.set(session, page);
  return page;
}

async function observePage(page, session) {
  const title = await page.title();
  const url = page.url();
  const text = await page.evaluate(() => document.body?.innerText || "");
  const elements = await page.evaluate(() =>
    Array.from(document.querySelectorAll("a,button,input,textarea,select"))
      .slice(0, 50)
      .map((element, index) => ({
        handle: String(index + 1),
        tag: element.tagName.toLowerCase(),
        text: element.innerText || element.value || element.getAttribute("aria-label") || "",
        selector: element.id ? `#${element.id}` : null,
      }))
  );
  return {
    session,
    url,
    title,
    text: text.slice(0, 8000),
    elements,
  };
}

async function handle(request) {
  const session = request.session || "default";
  switch (request.op) {
    case "open": {
      const page = await pageFor(session);
      await page.goto(request.url, { waitUntil: "domcontentloaded" });
      return observePage(page, session);
    }
    case "observe": {
      return observePage(await pageFor(session), session);
    }
    case "click": {
      const page = await pageFor(session);
      await page.click(request.selector);
      return observePage(page, session);
    }
    case "type": {
      const page = await pageFor(session);
      await page.fill(request.selector, request.text || "");
      return observePage(page, session);
    }
    case "press": {
      const page = await pageFor(session);
      await page.keyboard.press(request.key);
      return observePage(page, session);
    }
    case "screenshot": {
      const page = await pageFor(session);
      const screenshotPath =
        request.path || path.join(os.tmpdir(), `e-web-${Date.now()}.png`);
      await fs.mkdir(path.dirname(screenshotPath), { recursive: true });
      await page.screenshot({ path: screenshotPath, fullPage: true });
      return { ...(await observePage(page, session)), path: screenshotPath };
    }
    case "close": {
      const page = pages.get(session);
      if (page && !page.isClosed()) {
        await page.close();
      }
      pages.delete(session);
      return { session, closed: true };
    }
    default:
      throw new Error(`Unsupported browser operation: ${request.op}`);
  }
}

const rl = readline.createInterface({
  input: process.stdin,
  crlfDelay: Infinity,
});

rl.on("line", async (line) => {
  if (!line.trim()) return;
  let request;
  try {
    request = JSON.parse(line);
    const result = await handle(request);
    process.stdout.write(JSON.stringify({ id: request.id, ok: true, result }) + "\n");
  } catch (error) {
    const id = request?.id ?? null;
    process.stdout.write(
      JSON.stringify({ id, ok: false, error: error?.message || String(error) }) + "\n"
    );
  }
});

async function shutdown() {
  for (const page of pages.values()) {
    if (!page.isClosed()) {
      await page.close().catch(() => {});
    }
  }
  if (context) {
    await context.close().catch(() => {});
  }
  if (browser) {
    await browser.close().catch(() => {});
  }
}

process.on("SIGTERM", () => {
  shutdown().finally(() => process.exit(0));
});

process.on("SIGINT", () => {
  shutdown().finally(() => process.exit(0));
});

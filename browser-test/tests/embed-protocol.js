const assert = require("node:assert/strict");
const { test } = require("node:test");

const ENV = process.env.PYRET_ENV;
const SKIP = ENV !== "cpo" && "embed protocol pages require the CPO server (cpo env)";
const BASE_URL = (process.env.BASE_URL || "http://localhost:4999").replace(/\/+$/, "");
const COMPILER = process.env.PYRET_COMPILER || "pyret";
const COMPILER_PARAM = COMPILER === "pyret" ? "" : "&compiler=" + COMPILER;

async function waitForInitCount(page, count, timeout) {
  await page.waitForFunction(
    (n) => window.messages &&
      window.messages.filter((m) => m.data.protocol === "pyret" && m.data.data.type === "pyret-init").length === n,
    count,
    { timeout: timeout || 60000, polling: 100 }
  );
}

async function withFreshPage(getSession, body) {
  const browser = getSession().browser;
  const context = await browser.newContext();
  try {
    const page = await context.newPage();
    page.setDefaultTimeout(30000);
    return await body(page);
  } finally {
    await context.close();
  }
}

module.exports = function registerEmbedProtocol(getSession) {
  test("single instance: gets a pyret-init event on startup", { timeout: 60000, skip: SKIP }, async () => {
    await withFreshPage(getSession, async (page) => {
      await page.goto(BASE_URL + "/embed/embed1.html?" + BASE_URL + COMPILER_PARAM, { waitUntil: "domcontentloaded" });
      await waitForInitCount(page, 1);
    });
  });

  test("single instance: has definitions set after a simple reset", { timeout: 60000, skip: SKIP }, async () => {
    await withFreshPage(getSession, async (page) => {
      await page.goto(BASE_URL + "/embed/embed1.html?" + BASE_URL + COMPILER_PARAM, { waitUntil: "domcontentloaded" });
      await waitForInitCount(page, 1);
      await page.evaluate(() => {
        window.embedAPI.sendReset({
          definitionsAtLastRun: false,
          editorContents: "use context starter2024\n\nx = 'simple reset test'",
          replContents: "",
          interactionsSinceLastRun: [],
        });
      });
      const frame = page.frame({ name: "embed1" }) || (await page.locator("#embed1").elementHandle()).contentFrame();
      assert.ok(frame, "expected #embed1 iframe");
      await frame.waitForFunction(() => {
        const cm = document.querySelector(".CodeMirror");
        return !!(cm && cm.CodeMirror && cm.CodeMirror.getValue().includes("simple reset test"));
      }, undefined, { timeout: 30000, polling: 100 });
    });
  });

  test("two instances: gets a pyret-init event from each frame on startup", { timeout: 60000, skip: SKIP }, async () => {
    await withFreshPage(getSession, async (page) => {
      await page.goto(BASE_URL + "/embed/embed2.html?" + BASE_URL + COMPILER_PARAM, { waitUntil: "domcontentloaded" });
      await waitForInitCount(page, 2);
    });
  });

  test("two instances: propagates definitions changes across frames", { timeout: 60000, skip: SKIP }, async () => {
    await withFreshPage(getSession, async (page) => {
      await page.goto(BASE_URL + "/embed/embed2.html?" + BASE_URL + COMPILER_PARAM, { waitUntil: "domcontentloaded" });
      await waitForInitCount(page, 2);
      await page.evaluate(() => {
        const initState = {
          definitionsAtLastRun: false,
          editorContents: "use context starter2024\n\n",
          replContents: "",
          interactionsSinceLastRun: [],
        };
        window.frame1API.sendReset(initState);
        window.frame2API.sendReset(initState);
      });

      const frame1 = await (await page.locator("#embed1").elementHandle()).contentFrame();
      const frame2 = await (await page.locator("#embed2").elementHandle()).contentFrame();
      assert.ok(frame1 && frame2, "expected #embed1 and #embed2 iframes");

      await frame1.waitForFunction(() => {
        const cm = document.querySelector(".CodeMirror");
        return !!(cm && cm.CodeMirror && cm.CodeMirror.getValue().includes("starter2024"));
      }, undefined, { timeout: 30000, polling: 100 });
      await frame2.waitForFunction(() => {
        const cm = document.querySelector(".CodeMirror");
        return !!(cm && cm.CodeMirror && cm.CodeMirror.getValue().includes("starter2024"));
      }, undefined, { timeout: 30000, polling: 100 });

      await frame1.evaluate(() => {
        const cm = document.querySelector(".CodeMirror").CodeMirror;
        cm.replaceRange("x = 100\n", { line: 2, ch: 0 }, null);
      });

      await frame2.waitForFunction(() => {
        const cm = document.querySelector(".CodeMirror");
        return !!(cm && cm.CodeMirror && cm.CodeMirror.getValue().includes("x = 100"));
      }, undefined, { timeout: 30000, polling: 100 });
    });
  });
};

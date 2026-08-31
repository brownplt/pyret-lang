/*
 * Environment adapter: the embed API's embedded instance.
 *
 * Loads the embed host page (code.pyret.org/test-util/embed/embed1.html, served
 * at /embed/embed1.html), which creates an <iframe id="embed1"> pointing at
 * /editor#controlled=true and exposes window.embedAPI. In controlled mode the
 * editor waits for a `reset` before populating its contents
 * (beforePyret.js:1453), so we send one -- exactly like the upstream embed test
 * (code.pyret.org/test/embed.js) -- then hand back the iframe's editor frame.
 *
 * Needs the CPO server running at BASE_URL.
 */
const { launchChromium, wireBrowserLogs } = require("../shared/browser");
const { findEditorFrame } = require("../shared/find-frame");
const { resourceScope } = require("../shared/resource-scope");

const BASE_URL = process.env.BASE_URL || "http://localhost:4999";
// PYRET_COMPILER=ts makes the host page embed the TS-compiler flavor of the
// editor (embed1.js forwards it to the iframe as /editor?compiler=ts, the
// same knob the embed library's `compiler` config option uses).
const COMPILER = process.env.PYRET_COMPILER || "pyret";

async function setup() {
  const scope = resourceScope();
  try {
    const browser = await launchChromium();
    scope.add(() => browser.close());
    const page = await browser.newPage();
    wireBrowserLogs(page);
    page.setDefaultTimeout(60000);
    const compilerParam = COMPILER === "pyret" ? "" : "&compiler=" + COMPILER;
    await page.goto(BASE_URL + "/embed/embed1.html?" + BASE_URL + compilerParam, { waitUntil: "domcontentloaded", timeout: 120000 });

    // Wait for the embedded instance to announce itself (pyret-init).
    await page.waitForFunction(
      () => window.messages &&
        window.messages.filter((m) => m.data.protocol === "pyret" && m.data.data.type === "pyret-init").length === 1,
      undefined,
      { timeout: 60000, polling: 200 }
    );

    // Initialize the controlled editor with a runnable starter context.
    await page.evaluate(() =>
      window.embedAPI.sendReset({
        definitionsAtLastRun: false,
        editorContents: "use context starter2024\n\n",
        replContents: "",
        interactionsSinceLastRun: [],
      })
    );

    const frame = await findEditorFrame(page);
    return { page, frame, cleanup: scope.closeAll };
  } catch (e) {
    await scope.closeAll();
    throw e;
  }
}

module.exports = { setup, label: `embed API embedded instance (#embed1 iframe, ${COMPILER} compiler)` };

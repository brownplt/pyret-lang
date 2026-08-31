/*
 * Environment adapter: code.pyret.org /editor (the reference environment).
 *
 * Running the assertions here reproduces the upstream mocha suite's outcomes on
 * the real /editor page -- the role the old fidelity check played. Needs the CPO
 * server running at BASE_URL.
 */
const { launchChromium, wireBrowserLogs } = require("../shared/browser");
const { findEditorFrame } = require("../shared/find-frame");
const { resourceScope } = require("../shared/resource-scope");

const BASE_URL = process.env.BASE_URL || "http://localhost:4999";
// PYRET_COMPILER=ts loads the TypeScript-compiler flavor of the editor
// (the ?compiler=ts opt-in); default is the stock Pyret-hosted compiler.
const COMPILER = process.env.PYRET_COMPILER || "pyret";

async function setup() {
  const scope = resourceScope();
  try {
    const browser = await launchChromium();
    scope.add(() => browser.close());
    const page = await browser.newPage();
    wireBrowserLogs(page);
    page.setDefaultTimeout(60000);
    const query = COMPILER === "pyret" ? "" : "?compiler=" + COMPILER;
    await page.goto(BASE_URL + "/editor" + query, { waitUntil: "domcontentloaded", timeout: 120000 });
    const frame = await findEditorFrame(page);
    return { page, frame, cleanup: scope.closeAll };
  } catch (e) {
    await scope.closeAll();
    throw e;
  }
}

module.exports = { setup, label: `code.pyret.org /editor (reference, ${COMPILER} compiler)` };

/*
 * Environment adapter: the vscode extension's webview.
 *
 * Boots real VS Code for the Web (headless, via @vscode/test-web in
 * "server only" mode) with the vscode/ extension loaded as a dev extension over
 * the fixture workspace, drives it with Playwright to open algebra-2/test.arr in
 * the pyret-parley.cpo custom editor, and returns the webview's editor frame.
 *
 * Does NOT need the CPO server -- @vscode/test-web serves the editor assets out
 * of the built extension (vscode/dist/web/build/web).
 */
const path = require("path");
const { open } = require("@vscode/test-web");
const { launchChromium } = require("../shared/browser");
const { findEditorFrame } = require("../shared/find-frame");

const VSCODE_DIR = path.resolve(__dirname, "..", "..", "vscode");
const WORKSPACE = path.resolve(__dirname, "..", "vscode", "fixture-workspace");
const PORT = parseInt(process.env.VSCODE_TEST_PORT || "3198", 10);

// test-web serves `folderPath` as this virtual FS (see its mounts.js).
const MOUNT = "vscode-test-web://mount";
// The tab sits in algebra-2/ rather than at the workspace root so the suite's
// "../libraries/..." imports traverse to a real sibling directory INSIDE the
// workspace -- that traversal is the shape load-path tracking has to get right.
const FILE = "/algebra-2/test.arr";

async function setup() {
  const server = await open({
    browserType: "none",
    extensionDevelopmentPath: VSCODE_DIR,
    folderPath: WORKSPACE,
    port: PORT,
    quality: "stable",
    esm: true,
    printServerLog: false,
  });
  const endpoint = "http://localhost:" + PORT;

  const browser = await launchChromium();
  const page = await browser.newPage();
  page.setDefaultTimeout(60000);

  // Open the file directly via the workbench's `payload` query param, the same
  // mechanism vscode.dev uses for deep links -- no Explorer/Quick Open driving.
  // folderPath is served as the virtual FS 'vscode-test-web://mount', so a file
  // inside it addresses as mount/<relative path>. Opening it triggers the
  // pyret-parley.cpo custom editor.
  const payload = JSON.stringify([["openFile", MOUNT + FILE]]);
  await page.goto(endpoint + "?payload=" + encodeURIComponent(payload),
    { waitUntil: "domcontentloaded", timeout: 120000 });
  await page.waitForSelector(".monaco-workbench", { timeout: 120000 });

  // Bounded, and loud about WHY. A missing editor frame here used to be a silent
  // 120s poll ending in "not found within timeout", which says nothing about
  // whether the file opened at all, opened in the wrong editor, or opened in the
  // custom editor whose webview then failed to load. PYRET_BOOT_TIMEOUT dials it
  // down further while debugging.
  const bootTimeout = parseInt(process.env.PYRET_BOOT_TIMEOUT || "30000", 10);
  let frame;
  try {
    frame = await findEditorFrame(page, bootTimeout);
  } catch (e) {
    const diag = await page.evaluate(() => ({
      openTabs: Array.from(document.querySelectorAll(".tabs-container .tab"))
        .map((t) => (t.getAttribute("aria-label") || t.textContent || "").trim()),
      placeholder: (document.querySelector(".editor-placeholder") || {}).textContent || null,
      notifications: Array.from(document.querySelectorAll(".notification-list-item-message"))
        .map((n) => (n.textContent || "").trim()),
      explorerRows: Array.from(document.querySelectorAll(".monaco-list-row"))
        .map((r) => (r.getAttribute("aria-label") || "").trim()).filter(Boolean),
    }));
    diag.frameUrls = page.frames().map((f) => f.url());
    throw new Error(
      "vscode env: no editor frame after " + bootTimeout + "ms opening " + FILE + "\n" +
      JSON.stringify(diag, null, 2)
    );
  }
  return {
    page,
    frame,
    cleanup: async () => { await browser.close(); server.dispose(); },
  };
}

module.exports = { setup, label: "vscode pyret-parley.cpo webview" };

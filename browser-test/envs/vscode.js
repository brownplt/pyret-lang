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
const { launchChromium, wireBrowserLogs } = require("../shared/browser");
const { findEditorFrame } = require("../shared/find-frame");
const { resourceScope } = require("../shared/resource-scope");

const VSCODE_DIR = path.resolve(__dirname, "..", "..", "vscode");
// PYRET_COMPILER=ts opens the workspace whose .vscode/settings.json sets
// "pyret-parley.compiler": "ts", so the webview boots the TS-compiler flavor
// of the editor; the default workspace leaves the setting at its default.
const COMPILER = process.env.PYRET_COMPILER || "pyret";
const WORKSPACE = path.resolve(__dirname, "..", "vscode",
  COMPILER === "ts" ? "fixture-workspace-ts" : "fixture-workspace");
const PORT = parseInt(process.env.VSCODE_TEST_PORT || "3198", 10);

// test-web serves `folderPath` as this virtual FS (see its mounts.js).
const MOUNT = "vscode-test-web://mount";
// The tab sits in algebra-2/ rather than at the workspace root so the suite's
// "../libraries/..." imports traverse to a real sibling directory INSIDE the
// workspace -- that traversal is the shape load-path tracking has to get right.
const FILE = "/algebra-2/test.arr";

/*
 * "Markers" here are coming from vscode/src/diagnostics.ts, which only triggers
 * in development mode + a user-hidden setting.
 */
async function readLifecycleMarkers(page) {
  try {
    return await page.evaluate(() => {
      const PREFIX = "PYRET-DIAG";
      const hits = Array.from(document.querySelectorAll(".statusbar-item"))
        .map((s) => (s.textContent || "").trim())
        .filter((t) => t.indexOf(PREFIX) === 0);
      if (hits.length > 0) { return hits; }
      // The status bar's markup is VS Code's, not ours, so don't let a class
      // rename turn "the extension never activated" into "no markers found".
      // Fall back to any leaf element carrying the prefix, and say which path
      // produced the answer so an empty result is unambiguous.
      const anywhere = [];
      document.querySelectorAll("*").forEach((el) => {
        if (el.children.length !== 0) { return; }
        const t = (el.textContent || "").trim();
        if (t.indexOf(PREFIX) === 0) { anywhere.push(t); }
      });
      return anywhere.length > 0
        ? anywhere.map((t) => t + " (via full-document scan)")
        : ["<none: no " + PREFIX + " markers anywhere in the workbench DOM>"];
    });
  } catch (e) {
    return ["<unreadable: " + e.message + ">"];
  }
}

/*
 * Everything the harness can see about the workbench at a moment of interest:
 * which tabs are open, what the active editor group holds (a text editor, a
 * webview, or nothing -- the difference between "VS Code fell back to the
 * plain text editor" and "VS Code opened the custom editor and stalled"),
 * plus the extension lifecycle markers. Collected on every failure and on
 * every recovery step, so a log line is never just "it didn't work".
 */
async function workbenchSnapshot(page) {
  let snap;
  try {
    snap = await page.evaluate(() => ({
      openTabs: Array.from(document.querySelectorAll(".tabs-container .tab"))
        .map((t) => (t.getAttribute("aria-label") || t.textContent || "").trim()),
      placeholder: (document.querySelector(".editor-placeholder") || {}).textContent || null,
      notifications: Array.from(document.querySelectorAll(".notification-list-item-message"))
        .map((n) => (n.textContent || "").trim()),
      explorerRows: Array.from(document.querySelectorAll(".monaco-list-row"))
        .map((r) => (r.getAttribute("aria-label") || "").trim()).filter(Boolean),
      activeEditor: {
        textEditor: !!document.querySelector(".editor-instance .monaco-editor"),
        webviewElement: !!document.querySelector(".webview"),
        iframeCount: document.querySelectorAll("iframe").length,
      },
    }));
  } catch (e) {
    snap = { unreadable: e.message };
  }
  snap.frameUrls = page.frames().map((f) => f.url());
  snap.lifecycle = await readLifecycleMarkers(page);
  return snap;
}

async function setup() {
  const scope = resourceScope();
  try {
    const server = await open({
      browserType: "none",
      extensionDevelopmentPath: VSCODE_DIR,
      folderPath: WORKSPACE,
      port: PORT,
      quality: "stable",
      // Pinned build, not "latest stable": VS Code 1.130.0 (shipped 2026-07-22)
      // intermittently never resolves the custom editor in dev-extension mode
      // (see the commit message that added this line for the upstream trail).
      // This commit is 1.129.1, the last build this suite was green on.
      commit: "8a7abeba6e03ea3af87bfbce9a1b7e48fed567b8",
      esm: true,
      printServerLog: false,
    });
    scope.add(() => server.dispose());
    const endpoint = "http://localhost:" + PORT;

    const browser = await launchChromium();
    scope.add(() => browser.close());
    const page = await browser.newPage();
    wireBrowserLogs(page);
    page.setDefaultTimeout(60000);

    // Open the file directly via the workbench's `payload` query param, the same
    // mechanism vscode.dev uses for deep links -- no Explorer/Quick Open driving.
    // folderPath is served as the virtual FS 'vscode-test-web://mount', so a file
    // inside it addresses as mount/<relative path>. Opening it triggers the
    // pyret-parley.cpo custom editor.
    const payload = JSON.stringify([["openFile", MOUNT + FILE]]);
    const tNav = Date.now();
    await page.goto(endpoint + "?payload=" + encodeURIComponent(payload),
      { waitUntil: "domcontentloaded", timeout: 120000 });
    await page.waitForSelector(".monaco-workbench", { timeout: 120000 });
    const tWorkbench = Date.now();

    /*
     * NOTE(joe Aug '26): There are some annoying flakes that kept coming up in
     * CI that were clearly related to startup in the (maybe headless browser
     * only?) vscode about not actually getting the extension loaded on opening
     * a .arr file. Here, Claude came up with a plan to recover and try again
     *
     * Claude says: Two escalating recoveries, each loud in the log so CI keeps
     * measuring how often the stall happens and which nudge fixed it:
     *   1. close the stuck tab and reopen the file from the Explorer -- a
     *      fresh open against the SAME extension host. If this works, the
     *      provider was alive and only the first resolve was lost (the
     *      lost-wakeup theory, confirmed from inside).
     *   2. reload the whole page (the ?payload= re-fires) -- a fresh
     *      workbench, for whatever a re-open can't fix.
     *
     */
    const bootTimeout = parseInt(process.env.PYRET_BOOT_TIMEOUT || "120000", 10);
    const stallTimeout = Math.min(30000, Math.floor(bootTimeout / 4));
    const attempts = [];
    let frame = null;
    let via = "payload-open";
    try {
      frame = await findEditorFrame(page, stallTimeout);
    } catch (e) {
      attempts.push({ attempt: "payload-open", waitedMs: stallTimeout, snapshot: await workbenchSnapshot(page) });
      console.log("vscode env: STALL -- no editor frame " + stallTimeout + "ms after payload open; " +
        "trying close+reopen. " + JSON.stringify(attempts[0].snapshot.lifecycle));
    }

    if (!frame) {
      via = "close-and-reopen";
      try {
        const tab = page.locator(".tabs-container .tab", { hasText: "test.arr" }).first();
        await tab.hover();
        await tab.locator(".tab-actions .action-label").first().click();
        await page.waitForSelector(".tabs-container .tab", { state: "detached", timeout: 5000 });
        await page.click('.monaco-list-row[aria-label="test.arr"]');
        frame = await findEditorFrame(page, stallTimeout);
      } catch (e) {
        attempts.push({ attempt: "close-and-reopen", error: e.message.split("\n")[0],
          snapshot: await workbenchSnapshot(page) });
        console.log("vscode env: close+reopen did not produce a frame either; reloading the workbench.");
      }
    }

    if (!frame) {
      via = "page-reload";
      try {
        await page.reload({ waitUntil: "domcontentloaded", timeout: 60000 });
        await page.waitForSelector(".monaco-workbench", { timeout: 60000 });
        frame = await findEditorFrame(page, Math.min(60000, bootTimeout));
      } catch (e) {
        attempts.push({ attempt: "page-reload", error: e.message.split("\n")[0],
          snapshot: await workbenchSnapshot(page) });
      }
    }

    if (!frame) {
      const diag = {
        attempts,
        timings: { navToWorkbenchMs: tWorkbench - tNav, gaveUpAfterMs: Date.now() - tWorkbench },
      };
      throw new Error(
        "vscode env: no editor frame opening " + FILE + ", even after close+reopen and a reload\n" +
        JSON.stringify(diag, null, 2)
      );
    }
    // Print the markers on the way out of a SUCCESSFUL boot: what the healthy
    // ordering and timing look like is exactly the baseline a failing run has
    // to be read against -- and `via` says whether this boot needed a nudge,
    // which is the flake-rate signal to grep CI for.
    console.log("vscode env lifecycle (ok): " + JSON.stringify({
      via,
      markers: await readLifecycleMarkers(page),
      navToWorkbenchMs: tWorkbench - tNav,
      workbenchToFrameMs: Date.now() - tWorkbench,
    }));
    return { page, frame, cleanup: scope.closeAll };
  } catch (e) {
    await scope.closeAll();
    throw e;
  }
}

module.exports = { setup, label: `vscode pyret-parley.cpo webview (${COMPILER} compiler)` };

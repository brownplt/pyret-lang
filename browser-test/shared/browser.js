/*
 * browser.js -- one place to launch headless Chromium for the env adapters.
 *
 * If GOOGLE_CHROME_BINARY is set, use that Chrome build; otherwise fall back to
 * Playwright's bundled Chromium (`npx playwright install chromium`). This keeps
 * local runs (system Chrome) and CI (Playwright's Chromium) both working without
 * per-env duplication.
 */
const { chromium } = require("playwright");

// Relay the page's own story -- console, uncaught exceptions, failed
// requests -- into the runner's stdout, so it survives into CI logs. The
// editor reports its failures where a browser-side developer would look
// (e.g. beforePyret's bundle-load fallbacks console.error the reason and the
// exact URL), and until this relay existed that story was generated on every
// failing CI run and then discarded with the headless browser: the vscode x
// pyret runtime-load failure spent days as "doneRendering timed out" while
// the console held the actual error. Covers all frames of the page,
// including the vscode webview's.
function wireBrowserLogs(page) {
  const clip = (s) => {
    s = String(s);
    return s.length > 400 ? s.slice(0, 400) + " ...[clipped]" : s;
  };
  page.on("console", (msg) => {
    console.log("[browser " + msg.type() + "] " + clip(msg.text()));
  });
  page.on("pageerror", (err) => {
    console.log("[browser pageerror] " + clip(err));
  });
  page.on("requestfailed", (req) => {
    const f = req.failure();
    console.log("[browser requestfailed] " + req.method() + " " + clip(req.url()) +
      " -- " + (f ? f.errorText : "unknown"));
  });
  page.on("response", (resp) => {
    if (resp.status() >= 400) {
      console.log("[browser http " + resp.status() + "] " + clip(resp.url()));
    }
  });
}

async function launchChromium() {
  const opts = {
    headless: true,
    args: ["--no-sandbox", "--disable-dev-shm-usage"],
  };
  if (process.env.GOOGLE_CHROME_BINARY) {
    opts.executablePath = process.env.GOOGLE_CHROME_BINARY;
  }
  return chromium.launch(opts);
}

module.exports = { launchChromium, wireBrowserLogs };

/*
 * Environment adapter: the pyret-embed library driving the BUILT static embed
 * artifact, code.pyret.org/build/web/editor.embed.html -- no CPO server.
 *
 * --env=embed drives /editor#controlled=true through a running CPO server via
 * code.pyret.org's test-util host page, so it never loads editor.embed.html:
 * the file an embedding host actually deploys, rendered at build time from
 * .env.embed (BASE_URL=".", relative asset paths, POSTMESSAGE_ORIGIN="*"; see
 * code.pyret.org/make-template.js). This env serves that artifact from a plain
 * correct-MIME static server (shared/static-server.js) and embeds it through
 * the real pyret-embed library (embed/dist/pyret.js) from a host page that
 * mirrors the package's own examples (pages/embed-static-host.html) -- the
 * exact flow an npm consumer of pyret-embed runs.
 *
 * What this catches that --env=embed can't: breakage that lives only in the
 * built artifact or the library -- template variables mis-rendered at build
 * time, asset references that resolve at a server root but 404 under
 * relative/static hosting (e.g. a root-absolute /img/... sneaking back into
 * editor css), and pyret-embed API drift against the editor.
 *
 * Env vars:
 *   EMBED_STATIC_ROOT  override the served build dir
 *                      (default code.pyret.org/build/web)
 */
const path = require("path");
const fs = require("fs");
const { launchChromium, wireBrowserLogs } = require("../shared/browser");
const { findEditorFrame } = require("../shared/find-frame");
const { startStaticServer } = require("../shared/static-server");
const { resourceScope } = require("../shared/resource-scope");

const REPO_ROOT = path.resolve(__dirname, "..", "..");
const BUILD_ROOT = process.env.EMBED_STATIC_ROOT || path.join(REPO_ROOT, "code.pyret.org", "build", "web");
const EMBED_DIST = path.join(REPO_ROOT, "embed", "dist");
const PAGES_ROOT = path.resolve(__dirname, "..", "pages");

async function setup() {
  const artifact = path.join(BUILD_ROOT, "editor.embed.html");
  if (!fs.existsSync(artifact)) {
    throw new Error(
      "embed-static: " + artifact + " not found -- is code.pyret.org built? " +
        "(`npm run build` / `make web` produce it from src/web/editor.html + .env.embed)"
    );
  }
  const library = path.join(EMBED_DIST, "pyret.js");
  if (!fs.existsSync(library)) {
    throw new Error(
      "embed-static: " + library + " not found -- is pyret-embed built? " +
        "(`npm ci --ignore-scripts && npx webpack` in embed/ produce it)"
    );
  }

  const scope = resourceScope();
  try {
    const server = await startStaticServer({
      roots: [BUILD_ROOT, EMBED_DIST, PAGES_ROOT],
    });
    scope.add(() => server.close());

    const browser = await launchChromium();
    scope.add(() => browser.close());
    const page = await browser.newPage();
    wireBrowserLogs(page);
    page.setDefaultTimeout(60000);

    await page.goto(server.origin + "/embed-static-host.html", {
      waitUntil: "domcontentloaded",
      timeout: 120000,
    });

    // makeEmbedConfig resolves (and the host page sets window.embedAPI) once the
    // editor announces pyret-init and the initial state reset has been sent.
    await page.waitForFunction(() => !!window.embedAPI, undefined, {
      timeout: 60000,
      polling: 200,
    });

    const frame = await findEditorFrame(page);
    return { page, frame, cleanup: scope.closeAll };
  } catch (e) {
    await scope.closeAll();
    throw e;
  }
}

module.exports = {
  setup,
  label: "pyret-embed library against the static editor.embed.html artifact (no CPO server)",
};

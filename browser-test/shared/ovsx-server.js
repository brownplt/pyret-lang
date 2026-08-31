/*
 * ovsx-server.js -- a tiny static server that serves the built vscode extension
 * assets the way Open VSX serves them to the GitLab Web IDE, so we can reproduce
 * (and, once fixed, regression-test) issue #21 without deploying.
 *
 * GitLab Web IDE resolves an extension's webview resources to
 * `open-vsx.org/vscode/unpkg/<pub>/<name>/<ver>/extension/<path>`, and Open VSX
 * serves every file with two hostile properties (both confirmed by curl):
 *   1. `Content-Type: text/plain` + `X-Content-Type-Options: nosniff`, so the
 *      browser refuses to execute any `<script src>` / apply any `<link>` -->
 *      `$ is not defined`, `localSettings is not defined`, etc.
 *   2. a hard size cap (~15MB; 14.2MB serves, 20.1MB 503s) --> the 37MB
 *      `cpo-main.jarr.js` 503s and never loads.
 * `@vscode/test-web` (the normal --env=vscode) serves assets from a local static
 * server with correct MIME and no cap, which is why it never caught this.
 *
 * This server reproduces both. In `hostile` mode (default) it serves every asset
 * as text/plain+nosniff and 503s anything over the cap. In faithful mode
 * (OVSX_FAITHFUL=1) it serves correct content-types with no cap -- that mode
 * should behave like vscode.dev and boot the editor, which validates that the
 * harness plumbing itself is sound and that hostile serving is the only variable.
 *
 * The editor HTML itself is served (at `/__editor__`) as real text/html,
 * regardless of mode: in a real webview that HTML is INJECTED by the extension
 * host (via `pane.webview.html`), not fetched from Open VSX, so only its
 * sub-resources are subject to Open VSX. We mirror `getHtmlForWebview`
 * (pyretCPOWebEditor.ts) exactly: take the same built
 * `editor.selfcontained.html` the extension ships and fill its literal
 * `__PYRET_WEBVIEW_*__` placeholders with plain string replacement, with the
 * base URL pointed at this server.
 */
const http = require("http");
const fs = require("fs");
const path = require("path");

const TYPES = {
  ".js": "application/javascript",
  ".mjs": "application/javascript",
  ".css": "text/css",
  ".html": "text/html",
  ".json": "application/json",
  ".map": "application/json",
  ".png": "image/png",
  ".jpg": "image/jpeg",
  ".jpeg": "image/jpeg",
  ".gif": "image/gif",
  ".svg": "image/svg+xml",
  ".woff": "font/woff",
  ".woff2": "font/woff2",
  ".ttf": "font/ttf",
  ".eot": "application/vnd.ms-fontobject",
  ".wasm": "application/wasm",
  ".ico": "image/x-icon",
};

function contentType(p) {
  return TYPES[path.extname(p).toLowerCase()] || "application/octet-stream";
}

const EDITOR_PATH = "/__editor__";

/*
 * opts:
 *   assetRoot  - dir to serve (the extension's dist/web/build/web)
 *   hostile    - true (default): text/plain+nosniff + size cap; false: correct types
 *   capBytes   - hostile size cap (default 15MB)
 * returns { origin, editorPath, close }
 */
async function startOvsxServer(opts) {
  const assetRoot = path.resolve(opts.assetRoot);
  const hostile = opts.hostile !== false;
  const capBytes = opts.capBytes || 15 * 1024 * 1024;

  // The self-contained template (shell already inlined by the CPO build); this
  // is what the extension ships and renders.
  const templatePath = path.join(assetRoot, "views", "editor.selfcontained.html");
  if (!fs.existsSync(templatePath)) {
    throw new Error(
      "ovsx-server: editor.html template not found at " + templatePath +
        " -- is the extension built? (see browser-test/README.md prereqs)"
    );
  }
  const template = fs.readFileSync(templatePath, "utf8");

  let editorHtml = ""; // filled after listen (needs the origin)

  const server = http.createServer((req, res) => {
    let u;
    try {
      u = new URL(req.url, "http://localhost");
    } catch (e) {
      res.writeHead(400).end("bad request");
      return;
    }

    if (u.pathname === EDITOR_PATH) {
      // The injected webview HTML: always real text/html (not an Open VSX asset).
      res.writeHead(200, { "Content-Type": "text/html; charset=utf-8" });
      res.end(editorHtml);
      return;
    }

    const rel = decodeURIComponent(u.pathname).replace(/^\/+/, "");
    const filePath = path.resolve(assetRoot, rel);
    if (filePath !== assetRoot && !filePath.startsWith(assetRoot + path.sep)) {
      res.writeHead(403, { "Content-Type": "text/plain" }).end("forbidden");
      return;
    }
    fs.stat(filePath, (err, st) => {
      if (err || !st.isFile()) {
        res.writeHead(404, { "Content-Type": "text/plain" }).end("not found");
        return;
      }
      if (hostile && st.size > capBytes) {
        // Mimic Open VSX's 503 on oversized files (e.g. the 37MB bundle).
        res.writeHead(503, { "Content-Type": "text/html; charset=utf-8" });
        res.end("<!doctype html><html><body>Open VSX: resource too large</body></html>");
        return;
      }
      const headers = hostile
        ? { "Content-Type": "text/plain; charset=utf-8", "X-Content-Type-Options": "nosniff" }
        : { "Content-Type": contentType(filePath) };
      res.writeHead(200, headers);
      fs.createReadStream(filePath).pipe(res);
    });
  });

  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  const port = server.address().port;
  const origin = "http://127.0.0.1:" + port;

  // Fill the self-contained template's literal placeholders exactly as the
  // extension's getHtmlForWebview does (plain string replace). The shell is
  // already inlined; only the webview's later fetch of the .gz goes through the
  // hostile server. HASH is left "" so editor.html falls back to
  // document.location.hash, which the env sets (with initialState).
  editorHtml = template
    .split("__PYRET_WEBVIEW_BASE_URL__").join(origin)
    .split("__PYRET_WEBVIEW_HASH__").join("")
    .split("__PYRET_WEBVIEW_URL_FILE_MODE__").join("")
    .split("__PYRET_WEBVIEW_COMPILER__").join(process.env.PYRET_COMPILER || "pyret");

  return {
    origin,
    editorPath: EDITOR_PATH,
    close: () => new Promise((resolve) => server.close(() => resolve())),
  };
}

module.exports = { startOvsxServer };

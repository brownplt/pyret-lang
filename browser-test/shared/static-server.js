/*
 * static-server.js -- a minimal correct-MIME static file server, for envs that
 * exercise BUILT artifacts directly instead of going through the CPO server.
 *
 * Serves one or more root directories (first root containing the file wins)
 * on an ephemeral localhost port.
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
  ".xml": "application/xml",
  ".arr": "text/plain",
};

function contentType(p) {
  return TYPES[path.extname(p).toLowerCase()] || "application/octet-stream";
}

/*
 * opts:
 *   roots  - array of directories to serve, searched in order
 * returns { origin, close }
 */
async function startStaticServer(opts) {
  const roots = opts.roots.map((r) => path.resolve(r));

  const server = http.createServer((req, res) => {
    let pathname, delayMs;
    try {
      const u = new URL(req.url, "http://localhost");
      pathname = decodeURIComponent(u.pathname);
      delayMs = Math.min(Math.max(Number(u.searchParams.get("delay")) || 0, 0), 60000);
    } catch (e) {
      res.writeHead(400).end("bad request");
      return;
    }
    const rel = pathname.replace(/^\/+/, "");

    for (const root of roots) {
      const filePath = path.resolve(root, rel);
      if (filePath !== root && !filePath.startsWith(root + path.sep)) continue;
      let st;
      try {
        st = fs.statSync(filePath);
      } catch (e) {
        continue;
      }
      if (!st.isFile()) continue;
      // Most of our intended uses of importing by URL are cross-origin by
      // design, so serving one server for all tests that may be cross-origin is
      // a useful and realistic setup.
      const send = () => {
        res.writeHead(200, {
          "Content-Type": contentType(filePath),
          "Access-Control-Allow-Origin": "*",
        });
        fs.createReadStream(filePath).pipe(res);
      };
      if (delayMs > 0) setTimeout(send, delayMs); else send();
      return;
    }
    res.writeHead(404, {
      "Content-Type": "text/plain",
      "Access-Control-Allow-Origin": "*",
    }).end("not found: " + pathname);
  });

  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  return {
    origin: "http://127.0.0.1:" + server.address().port,
    close: () => new Promise((resolve) => server.close(() => resolve())),
  };
}

module.exports = { startStaticServer };

/*
  Generates the browserify entry for the TypeScript compiler bundle
  (build/ts-compiler.js, gzipped into build/web/js/ts-compiler.gz.js for
  serving). The TS compiler reuses a handful of
  runtime-independent AMD-style JS modules (tokenizer, generated parser,
  js-numbers, type-util, jglr); in node it loads them from disk, in the
  browser their sources are registered up front via the amd hook.

  Usage: node src/scripts/make-ts-compiler-entry.js <output-file>
  Requires `make ts-compiler` to have been run in pyret/ first (for
  build/ts-compiler/browser.js and the generated parser).
*/
const fs = require("fs");
const path = require("path");

const out = process.argv[2];
if (!out) {
  console.error("usage: node make-ts-compiler-entry.js <output-file>");
  process.exit(1);
}

const pyretRoot = path.resolve(__dirname, "..", "..", "pyret");
const browserMain = path.join(pyretRoot, "build", "ts-compiler", "browser.js");

const amdModules = {
  "pyret-base/js/pyret-tokenizer": "src/js/base/pyret-tokenizer.js",
  "pyret-base/js/pyret-parser": "build/ts-compiler/js/pyret-parser.js",
  "pyret-base/js/js-numbers": "src/js/base/js-numbers.js",
  "pyret-base/js/type-util": "src/js/base/type-util.js",
  "jglr/jglr": "lib/jglr/jglr.js",
  "jglr/rnglr": "lib/jglr/rnglr.js",
  "jglr/cyclicJSON": "lib/jglr/cyclicJSON.js"
};

// Sources must be registered before browser.js is required: some interop
// modules (e.g. js-numbers) call amdRequire at import time.
const amdMain = path.join(pyretRoot, "build", "ts-compiler", "interop", "amd.js");
let entry = "";
entry += "var amd = require(" + JSON.stringify(amdMain) + ");\n";
for (const [name, rel] of Object.entries(amdModules)) {
  const file = path.join(pyretRoot, rel);
  const src = fs.readFileSync(file, "utf8");
  entry += "amd.registerModuleSource(" + JSON.stringify(name) + ", " + JSON.stringify(src) + ");\n";
}
entry += "module.exports = require(" + JSON.stringify(browserMain) + ");\n";

fs.mkdirSync(path.dirname(path.resolve(out)), { recursive: true });
fs.writeFileSync(out, entry);
console.log("Wrote " + out + " (" + entry.length + " bytes)");

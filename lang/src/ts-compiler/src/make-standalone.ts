/*
  Ported from: src/js/trove/make-standalone.js

  Byte-identical concatenation to the JS version:
    amd_loader.js
    + deps-file contents
    + each config["raw-js"] file (with $PYRET substitution / baseDir resolution)
    + 'define("program", [deps..., "pyret-base/js/runtime"], function() {\nreturn '
    + body (printed via print-ugly-source)
    + '\n});\n'
    + standalone-file (handalone.js)
  followed by fsync.
*/

import * as fs from 'fs';
import * as path from 'path';

const READ_OPTIONS = { encoding: 'utf8' as const };

// TODO(joe): figure our where web-standalone-template should go
// (In the JS original this is declared inside makeStandalone, which makes
// makeHtmlFile's reference to it a scoping bug; hoisted here so that
// makeHtmlFile works as intended.)
const HTML_TEMPLATE = "src/scripts/web-standalone-template.html";

export function makeHtmlFile(bundledJSFile: string, outfile: string): boolean {
  const template = fs.readFileSync(HTML_TEMPLATE, READ_OPTIONS);

  const relativePath = path.relative(path.dirname(outfile), bundledJSFile);
  const filtered = template.replace("{{{compiled-jarr-file}}}", relativePath);

  fs.writeFileSync(outfile, filtered);
  return true;
}

export interface MakeStandaloneOptions {
  standaloneFile: string;
  depsFile: string;
  thisPyretDir: string;
  baseDir: string;
}

/*
  deps: the requirejs dependencies of the program (strings)

  body: the JS-compiled program (anything with print-ugly-source, e.g. a
  CompiledCodePrinter or a JExpr)

  configJSON: a JSON string to parse and use as a configuration option
  (must contain "out", "use-raw-files", "raw-js" and "baseUrl")

  options.standaloneFile: file template for the standalone (usually
  src/js/base/handalone.js)

  options.depsFile: file that contains the builtin npm/node dependencies,
  either as uses of "require" (i.e. dynamically linked) or as the output of
  `browserify` (i.e. statically linked)
*/
export function makeStandalone(
  deps: string[],
  body: { printUglySource(printer: (s: string) => void): void },
  configJSON: string,
  options: MakeStandaloneOptions
): boolean {
  const standaloneFile = options.standaloneFile;
  const depsFile = options.depsFile;
  const thisPyretDir = options.thisPyretDir;
  const baseDir = options.baseDir;

  const AMD_LOADER = path.join(thisPyretDir, "js/amd_loader.js");

  // TODO(joe): make sure this gets embedded correctly in the built version; can't
  // necessarily rely on this path
  const config = JSON.parse(configJSON);
  const handalone = fs.readFileSync(standaloneFile, READ_OPTIONS);
  const depsArr = [...deps];
  depsArr.push("pyret-base/js/runtime");
  const depsStrs = depsArr.map(function (d) { return '"' + d + '"'; });
  const depsLine = "[" + depsStrs.join(",") + "]";

  if (!("out" in config)) {
    throw new Error("make-standalone config must have an 'out' field");
  }
  const realOut = config.out;
  if (!config["use-raw-files"]) {
    throw new Error("Cannot not use raw-files! RequireJS is gone");
  }
  const outFile = fs.openSync(realOut, "w");

  // Write the amd loader first
  const loaderContents = fs.readFileSync(AMD_LOADER, READ_OPTIONS);
  fs.writeSync(outFile, loaderContents);

  // Now either write the file containing all dependencies or the file which
  // just defines() the dependencies.

  const dependencyCode = fs.readFileSync(depsFile, READ_OPTIONS);
  fs.writeSync(outFile, dependencyCode);

  const filesToFetch = config["raw-js"];
  // Sorted so concatenation order doesn't depend on config-dict iteration
  // order (kept in lockstep with src/js/trove/make-standalone.js)
  Object.keys(filesToFetch).sort().forEach(function (f) {
    let filename: string;
    if (filesToFetch[f].indexOf("$PYRET") !== -1) {
      filename = filesToFetch[f].replace("$PYRET", thisPyretDir);
    } else if (!path.isAbsolute(filesToFetch[f])) {
      filename = path.resolve(path.join(baseDir, filesToFetch[f]));
    } else {
      filename = filesToFetch[f];
    }

    const contents = fs.readFileSync(filename, { encoding: 'utf8' });
    fs.writeSync(outFile, contents);
  });
  fs.writeSync(outFile, "define(\"program\", " + depsLine + ", function() {\nreturn ");
  const writeRealOut = function (str: string) {
    fs.writeSync(outFile, str);
  };
  body.printUglySource(writeRealOut);
  fs.writeSync(outFile, "\n});\n");
  fs.writeSync(outFile, handalone);
  fs.fsyncSync(outFile);
  fs.closeSync(outFile);
  return true;
}

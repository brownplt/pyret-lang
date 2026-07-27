/*
 * dispatch.js -- map a loaded spec (from load-cpo-specs) to the matching
 * assertion in cpo-assertions. Throws (AssertionError for content, the spec's
 * own ProceduralError otherwise) if it fails; returns normally if it passes.
 */
const A = require("./cpo-assertions");
const { ProceduralError } = require("./errors");

async function runSpec(page, s) {
  switch (s.kind) {
    case "checkBlocks":
      return A.testRunsAndHasCheckBlocks(page, s.code, s.specs, s.options);
    case "errorString":
      return A.testErrorRendersString(page, s.code, s.expected, s.options);
    case "repl":
      return A.testRunAndUseRepl(page, s.code, s.repl, s.options);
    case "allTestsPass":
      return A.runAndCheckAllTestsPassed(page, s.code, s.name, 20000);
    case "pyretFile":
      // tables.js uses doForEachPyretFile with a table-specific assertion.
      if (s.suite === "tables") {
        return A.checkTableRendersCorrectly(page, s.code, s.program, s.baseTimeout || 900000);
      }
      return A.runAndCheckAllTestsPassed(page, s.code, s.program, s.baseTimeout || 900000);
    default:
      throw new ProceduralError("unsupported spec kind: " + s.kind);
  }
}

// A reasonable per-test timeout by spec kind (charts/tables render slowly).
// A spec's own options.timeout (e.g. url-imports' remote fetches) wins.
function specTimeout(s) {
  if (s.options && s.options.timeout) return s.options.timeout + 30000;
  if (s.kind === "pyretFile" || s.kind === "repl") return s.baseTimeout || 900000;
  return 60000;
}

module.exports = { runSpec, specTimeout };

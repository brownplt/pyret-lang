/*
 * load-cpo-specs.js
 *
 * Extracts the EXACT test inputs ("specs") that code.pyret.org's mocha suite
 * feeds to its assertions, WITHOUT copying or editing any file under
 * code.pyret.org/.
 *
 * It does this by `require`-ing the spec files under browser-test/cpo/test/
 * (errors.js, check-blocks.js, type-check.js, ...) with the
 * mocha globals (describe/it/before/...) and the `../test-util/util.js` module
 * stubbed out by *recording shims*. When the original file runs its
 * registration code -- e.g.
 *
 *     tests.forEach(function(t) {
 *       tester.testRunsAndHasCheckBlocks(it, t[0], t[1], t[2]);
 *     });
 *
 * our shim for `testRunsAndHasCheckBlocks` records `{name, code, specs}` instead
 * of registering a selenium test. The result is the byte-for-byte same list of
 * (program text, expected content) tuples the upstream suite checks -- so when
 * we later run the SAME assertions against the embed iframe / vscode webview, we
 * are provably checking the same things on the same inputs.
 *
 * Nothing here is specific to a browser/runner; it is pure Node.
 */

const path = require("path");
const fs = require("fs");
const Module = require("module");

const SUITE_DIR = path.resolve(__dirname, "../cpo");
const TESTS_DIR = path.resolve(__dirname, "../tests");

// The moved CPO test files use require("../test-util/util.js"). We intercept
// that require so it returns a recording stub instead of a selenium harness.
function withStubbedUtil(recorder, body) {
  const realLoad = Module._load;

  // Recording shim that mimics util.js's exported registration helpers. Each one
  // records the tuple it was given (the same tuple the real helper would turn
  // into an assertion) and is otherwise a no-op.
  const stub = {
    // ---- registration helpers: these define what gets asserted ----
    testErrorRendersString: (it, name, toEval, expectedString, options) =>
      recorder.push({ kind: "errorString", name, code: String(toEval), expected: expectedString, options }),
    testRunsAndHasCheckBlocks: (it, name, toEval, specs, options) =>
      recorder.push({ kind: "checkBlocks", name, code: String(toEval), specs, options }),
    testRunAndAllTestsPass: (it, name, toEval, options) =>
      recorder.push({ kind: "allTestsPass", name, code: String(toEval), options }),
    testRunAndUseRepl: (it, name, toEval, toRepl, options) =>
      recorder.push({ kind: "repl", name, code: String(toEval), repl: toRepl, options }),

    // doForEachPyretFile(it, name, base, testFun, baseTimeout): the file-driven
    // suites (charts, images, tables, world). We re-read the directory exactly
    // like the original and record each .arr program; the assertion kind is
    // recorded from `name` (chart/image/tables/world) so the runner can dispatch
    // to the matching assertion.
    doForEachPyretFile: (it, name, base, testFun, baseTimeout) => {
      const dir = path.resolve(SUITE_DIR, base);
      const programs = fs.readdirSync(dir).filter((p) => p.endsWith(".arr"));
      programs.forEach((program) => {
        recorder.push({
          kind: "pyretFile",
          suite: name,
          base,
          program,
          code: String(fs.readFileSync(path.join(dir, program))),
          baseTimeout,
        });
      });
    },

    // ---- everything else the test files touch: harmless no-ops ----
    setup() {}, teardown() {},
    setupMulti: () => function () {}, teardownMulti() {},
    pyretLoaded() {}, waitForPyretLoad() {}, evalPyret() {},
    runAndCheckAllTestsPassed() {}, checkTableRendersCorrectly() {},
    checkWorldProgramRunsCleanly() {}, loadAndRunPyret() {},
    evalDefinitionsAndWait() {}, evalDefinitions() {}, evalPyretNoError() {},
    waitForBreakButton() {}, waitForNoPrompt() {}, waitForEditorContent() {},
    waitForWorldProgram() {},
  };

  Module._load = function (request, parent, isMain) {
    if (/(?:^|[\\/])test-util[\\/]util(?:\.js)?$/.test(request)) {
      return stub;
    }
    return realLoad.call(Module, request, parent, isMain);
  };

  try {
    return body();
  } finally {
    Module._load = realLoad;
  }
}

// Stub the mocha BDD globals so requiring a test file executes its top-level
// describe() body (where registration happens) but registers nothing real.
function withStubbedMocha(body) {
  const g = global;
  const saved = {};
  const names = ["describe", "xdescribe", "it", "xit", "before", "after", "beforeEach", "afterEach"];
  for (const n of names) saved[n] = g[n];

  const describe = (title, fn) => { if (typeof fn === "function") fn.call({ timeout() {} }); };
  describe.only = describe; describe.skip = () => {};
  const xdescribe = () => {};
  const it = () => {}; it.only = () => {}; it.skip = () => {};
  const xit = () => {};
  const hook = () => {};

  Object.assign(g, { describe, xdescribe, it, xit, before: hook, after: hook, beforeEach: hook, afterEach: hook });
  try {
    return body();
  } finally {
    for (const n of names) {
      if (saved[n] === undefined) delete g[n];
      else g[n] = saved[n];
    }
  }
}

/**
 * Load the specs registered by one upstream test file (e.g. "errors.js").
 * Returns an array of recorded spec objects. cwd is temporarily set to CPO_DIR
 * because the upstream files use cwd-relative fs.readFileSync paths.
 */
function loadSpecsFromFile(testFileName) {
  const testFilePath = path.join(SUITE_DIR, "test", testFileName);
  const recorder = [];
  const prevCwd = process.cwd();
  process.chdir(SUITE_DIR);
  try {
    delete require.cache[require.resolve(testFilePath)];
    withStubbedMocha(() => withStubbedUtil(recorder, () => {
      require(testFilePath);
    }));
  } finally {
    process.chdir(prevCwd);
    delete require.cache[require.resolve(testFilePath)];
  }
  return recorder;
}

/**
 * Load one suite's specs, whichever kind of source names them.
 *
 * A bare filename ("errors.js") is a spec file in browser-test/cpo/test/, read
 * through the recording shims above. A relative specifier ("./big-programs.js")
 * is a harness-local module that exports specs(). Node's own rule for
 * telling those apart (bare vs relative) is the rule used here, so a suite
 * entry reads like the require() it stands in for; local paths resolve against
 * tests/, where the suite table lives.
 *
 * Both kinds yield the same {kind, name, code, options} spec objects, so no
 * caller has to know which one a suite came from -- that is the point of
 * having one entry form rather than two.
 */
function loadSpecs(source) {
  const isRelative = source.startsWith("./") || source.startsWith("../");
  if (isRelative) return require(path.join(TESTS_DIR, source)).specs();
  return loadSpecsFromFile(source);
}

module.exports = { loadSpecs, loadSpecsFromFile, SUITE_DIR, CPO_DIR: SUITE_DIR };

// CLI: `node load-cpo-specs.js errors.js` prints a summary.
if (require.main === module) {
  const file = process.argv[2] || "errors.js";
  const specs = loadSpecs(file);
  const byKind = {};
  for (const s of specs) byKind[s.kind] = (byKind[s.kind] || 0) + 1;
  console.log(`Loaded ${specs.length} specs from test/${file}:`, byKind);
  console.log(JSON.stringify(specs.slice(0, 3), null, 2));
}

/*
 * cpo-assertions.js
 *
 * Reproduces the upstream assertion functions from
 * code.pyret.org/test-util/util.js against an abstract `page` adapter. The
 * DOM-level work is delegated to window.PA (shared/page-assertions.js), a
 * line-for-line port of the util.js predicates.
 *
 * Failure taxonomy (see shared/errors.js):
 *   - CONTENT checks -- the things actually under test -- use node:assert, so a
 *     wrong rendering surfaces as an AssertionError (with a value diff).
 *   - PROCEDURAL problems -- couldn't install the program, a value never
 *     rendered, the REPL errored unexpectedly -- throw ProceduralError.
 *   - Setup waits (page.waitFor) throw Playwright TimeoutError, also procedural.
 *
 * `page` adapter contract:
 *   page.inject()                  -> Promise
 *   page.eval(exprString)          -> Promise<any>   ; evaluate expr in editor frame
 *   page.waitFor(exprString, ms)   -> Promise         ; poll until truthy / throw on timeout
 */
const assert = require("node:assert/strict");
const { ProceduralError } = require("./errors");

// mirrors util.js ensureRendered -- an internal render error means the run
// itself broke, so this is procedural rather than a content mismatch.
function ensureRendered(text) {
  if (text.indexOf("One or more internal errors") > -1) {
    throw new ProceduralError('internal error while rendering output: "' + text + '"');
  }
}

// Set the definitions and confirm CM actually holds them before continuing.
// The vscode custom editor binds CM to the TextDocument and may asynchronously
// push contents back, so a single setDefinitions can be overwritten; we re-apply
// until it sticks. (On cpo/embed the first attempt already matches.)
async function setDefinitionsConfirmed(page, code) {
  await page.inject();
  for (let i = 0; i < 20; i++) {
    await page.eval("window.PA.setDefinitions(" + JSON.stringify(code) + ")");
    if ((await page.eval("window.PA.cmValue()")) === code) return;
    await new Promise((r) => setTimeout(r, 100));
  }
  throw new ProceduralError("could not install definitions into the editor (doc-sync race)");
}

// mirrors util.setDefinitionsEvalAndWait (set definitions, run, wait for break btn)
async function setDefinitionsRunAndWait(page, code, options) {
  await setDefinitionsConfirmed(page, code);
  await page.eval("window.PA.clearOutput()");
  if (options && options.typeCheck) {
    await page.eval("window.PA.runTypeCheck()");
  } else {
    await page.eval("window.PA.run()");
  }
  await page.waitFor("window.PA.breakDone()", (options && options.timeout) || 30000);
}

// Warm up the Pyret runtime once, in setup. The FIRST program run in a freshly
// loaded editor pays a large one-time cost (runtime warmup + first check-results
// render) -- ~20s under CI's headless Chromium, vs ~120ms once warm. Absorbing
// it here keeps every actual test fast and within the tight per-test waits,
// deterministically (rather than padding timeouts or retrying).
async function warmUp(page) {
  await setDefinitionsConfirmed(page, "check:\n  1 is 1\nend");
  await page.eval("window.PA.clearOutput()");
  await page.eval("window.PA.run()");
  await page.waitFor("window.PA.breakDone()", 120000);
  await page.waitFor("window.PA.doneRendering()", 120000);
}

// mirrors util.checkAllTestsPassed -- CONTENT: "Looks shipshape" must be present.
async function checkAllTestsPassed(page, name, timeout) {
  await page.inject();
  await page.waitFor("window.PA.testingSummaryPresent()", timeout || 20000);
  await page.waitFor("window.PA.doneRendering()", 20000);
  const res = await page.eval("window.PA.shipshapeResult()");
  assert.ok(
    res.shipshape,
    'expected all tests to pass ("Looks shipshape"), but these blocks failed: ' +
      JSON.stringify(res.failures, null, 2)
  );
}

// mirrors util.runAndCheckAllTestsPassed
async function runAndCheckAllTestsPassed(page, code, name, timeout) {
  await setDefinitionsConfirmed(page, code);
  await page.eval("window.PA.clearOutput()");
  await page.eval("window.PA.run()");
  // Wait for the run to actually finish before reading, so a reused frame can't
  // observe the previous program's results.
  await page.waitFor("window.PA.breakDone()", timeout || 900000);
  return checkAllTestsPassed(page, name, timeout);
}

// mirrors util.testErrorRendersString -- CONTENT: error output contains `expected`.
async function testErrorRendersString(page, code, expected, options) {
  await setDefinitionsRunAndWait(page, code, options);
  await page.waitFor("window.PA.compileErrorPresent()", 6000);
  await page.eval("window.PA.removeOutputCodeMirrors()");
  const text = await page.eval("window.PA.outputText()");
  ensureRendered(text);
  assert.ok(
    text.indexOf(expected) !== -1,
    "error output should contain " + JSON.stringify(expected) + ", but was:\n" + text
  );
}

// mirrors util.testRunsAndHasCheckBlocks -- CONTENT: block/test counts and the
// substrings each test must contain.
async function testRunsAndHasCheckBlocks(page, code, specs, options) {
  await setDefinitionsRunAndWait(page, code, options);
  await page.waitFor("window.PA.doneRendering()", 20000);
  await page.eval("window.PA.removeOutputCodeMirrors()");
  const specLens = specs.map((s) => s.length);
  const blocks = await page.eval("window.PA.collectCheckBlocks(" + JSON.stringify(specLens) + ")");

  assert.strictEqual(blocks.length, specs.length, "number of check blocks rendered");
  blocks.forEach((b, i) => {
    assert.strictEqual(b.length, specs[i].length, "number of tests in check block " + i);
    b.forEach((text, j) => {
      ensureRendered(text);
      specs[i][j].forEach((must) => {
        assert.ok(
          text.indexOf(must) !== -1,
          "check block " + i + " test " + j + " should contain " + JSON.stringify(must) +
            ", but was: " + JSON.stringify(text)
        );
      });
    });
  });
}

// mirrors util.evalPyretNoError: submit code at the REPL, wait for a new result
// child of #output, and return its .replOutput/.replTextOutput texts. A result
// that isn't an echo-container/trace means the REPL errored -> procedural.
async function evalAtReplNoError(page, code) {
  await page.inject();
  await page.waitFor("window.PA.replPromptVisible()", 15000);
  const before = await page.eval("window.PA.outputChildCount()");
  await page.eval("window.PA.evalAtRepl(" + JSON.stringify(code) + ")");
  await page.waitFor("window.PA.outputChildCount() > " + before, 15000);
  // Let the value render. Assignments legitimately produce no output, so cap the
  // wait rather than require non-empty output.
  let res = null;
  for (let i = 0; i < 30; i++) {
    res = await page.eval("window.PA.lastOutputChild()");
    if (res && res.outputs.length > 0) break;
    await new Promise((r) => setTimeout(r, 200));
  }
  if (!res || !(res.class === "echo-container" || res.class === "trace")) {
    throw new ProceduralError("REPL did not run cleanly for: " + code);
  }
  return res.outputs;
}

// mirrors util.testRunAndUseRepl -- CONTENT: each REPL result contains the
// expected substring. (An empty `expected` is trivially contained, matching
// util.js, so assignment statements with no output pass.)
async function testRunAndUseRepl(page, code, toRepl, options) {
  await setDefinitionsRunAndWait(page, code, options);
  for (const tr of toRepl) {
    const outputs = await evalAtReplNoError(page, tr[0]);
    const got = outputs[0] || "";
    assert.ok(
      got.indexOf(tr[1]) !== -1,
      "REPL " + JSON.stringify(tr[0]) + " output " + JSON.stringify(got) +
        " should contain " + JSON.stringify(tr[1])
    );
  }
}

// Submit code at the REPL, wait for a new result child, then poll a reader
// expression until it returns a non-null value (the value renders async).
async function evalAtReplThenRead(page, code, readerExpr) {
  await page.waitFor("window.PA.replPromptVisible()", 15000);
  const before = await page.eval("window.PA.outputChildCount()");
  await page.eval("window.PA.evalAtRepl(" + JSON.stringify(code) + ")");
  await page.waitFor("window.PA.outputChildCount() > " + before, 15000);
  let v = null;
  for (let i = 0; i < 50; i++) {
    v = await page.eval(readerExpr);
    if (v !== null && v !== undefined) break;
    await new Promise((r) => setTimeout(r, 200));
  }
  return v;
}

// mirrors util.checkTableRendersCorrectly -- CONTENT: each table cell's rendered
// HTML equals the rendered HTML of the corresponding value expression.
async function checkTableRendersCorrectly(page, code, name, timeout) {
  await setDefinitionsConfirmed(page, code);
  await page.eval("window.PA.clearOutput()");
  await page.eval("window.PA.run()");
  await page.waitFor("window.PA.breakDone()", timeout || 900000);
  await page.waitFor("window.PA.tablePre() !== null", 20000);
  const tests = JSON.parse(await page.eval("window.PA.tablePre()"));
  if (!tests.length) throw new ProceduralError("table program produced no cell specs");

  for (const t of tests) {
    const cellHTML = await evalAtReplThenRead(
      page, t.table, "window.PA.lastReplTableCellHTML(" + t.row + "," + t.col + ")"
    );
    const valHTML = await evalAtReplThenRead(page, t.val, "window.PA.lastReplOutputHTML()");
    assert.strictEqual(
      cellHTML, valHTML,
      "table cell (row " + t.row + ", col " + t.col + ") should render like " + t.val
    );
  }
  // NOTE: util.checkTableRendersCorrectly has a checkAllTestsPassed(...) call
  // after its `return maybeTest.then(...)`, i.e. unreachable dead code. We match
  // that: the assertion is the per-cell render comparison.
}

module.exports = {
  ensureRendered,
  warmUp,
  checkTableRendersCorrectly,
  setDefinitionsRunAndWait,
  checkAllTestsPassed,
  runAndCheckAllTestsPassed,
  testErrorRendersString,
  testRunsAndHasCheckBlocks,
  evalAtReplNoError,
  testRunAndUseRepl,
};

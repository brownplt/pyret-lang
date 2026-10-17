/*
 * fs-rpc-failures.js -- in VS Code, a file import whose filesystem RPC fails
 * must end the run with an error, not wait forever.
 *
 * `import file(...)` reads through the extension host's fs RPC while the
 * Pyret stack is paused. When that RPC came back as an exception, nothing on
 * the webview side handled the rejected promise, so the run never resumed and
 * the editor looked like an infinite loop. The field report was an EPERM
 * (macOS privacy protection on ~/Desktop); a missing file takes the same path
 * and is reproducible in test-web's in-memory filesystem.
 *
 * The fixtures live in vscode/fixture-workspace*\/libraries, a sibling of the
 * open tab's directory, so these imports go through "../".
 *
 * Only the vscode env has a filesystem behind file(); elsewhere these skip.
 */
const assert = require("node:assert/strict");
const { test } = require("node:test");
const { IDLE, runProgram, outputHas, outputText, waitOrFail } = require("./helpers");

const ENV = process.env.PYRET_ENV;
const SKIP = ENV !== "vscode" && "file() imports need the vscode env's filesystem";

module.exports = function registerFsRpcFailures(getSession) {
  // The control: without it, the failure cases below could pass because
  // file() imports error for some unrelated reason.
  test("a file import that exists loads", { timeout: 90000, skip: SKIP }, async () => {
    const page = getSession().page;
    await runProgram(page, 'import file("../libraries/small.arr") as S\n\nS.small-value\n');
    await waitOrFail(page, `${outputHas("from-small-arr")} && ${IDLE}`, 60000,
      "importing an existing file did not produce its value");
  });

  test("a file import that cannot be read ends the run with an error", { timeout: 90000, skip: SKIP }, async () => {
    const page = getSession().page;
    await runProgram(page, 'import file("../libraries/does-not-exist.arr") as S\n\nS.anything\n');
    await waitOrFail(page, `${outputHas("does-not-exist.arr")} && ${IDLE}`, 60000,
      "a failed read of an imported file should end the run with an error naming the file");
  });

  test("a file import that does not parse ends the run with a parse error", { timeout: 90000, skip: SKIP }, async () => {
    const page = getSession().page;
    await runProgram(page, 'import file("../libraries/bad-syntax.arr") as S\n\nS.broken\n');
    await waitOrFail(page, `${outputHas("bad-syntax.arr")} && ${IDLE}`, 60000,
      "an imported file with a syntax error should end the run with an error naming the file");
    const shown = await outputText(page);
    assert.ok(/understand|parse/i.test(shown),
      "expected a parse error, but the editor shows: " + JSON.stringify(shown.slice(0, 300)));
  });

  test("the extension answers an RPC for an unknown module", { timeout: 30000, skip: SKIP }, async () => {
    const page = getSession().page;
    const outcome = await page.eval(
      "Promise.race([" +
      "  window.MESSAGES.sendRpc('no-such-module', 'nope', []).then(" +
      "    function(v) { return 'resolved: ' + String(v); }," +
      "    function(e) { return 'rejected: ' + String(e); })," +
      "  new Promise(function(r) { setTimeout(function() { r('no response'); }, 10000); })" +
      "])");
    assert.match(outcome, /^rejected: .*Unknown module/);
  });
};

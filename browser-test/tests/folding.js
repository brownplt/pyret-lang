const assert = require("node:assert/strict");
const { test } = require("node:test");
const { IDLE, install, waitOrFail } = require("./helpers");

const PROGRAM = `use context starter2024

fun double(n :: Number) -> Number:
  n + n
end

check:
  double(3) is 6
end
`;

module.exports = function registerFolding(getSession) {
  test("Run keeps a function folded and clears its old highlight", { timeout: 120000 }, async () => {
    const page = getSession().page;
    await install(page, PROGRAM);
    await page.eval("window.PA.clearOutput()");

    // Put the highlight inside the fold so both marks cover the same character.
    await page.eval(`(() => {
      const cm = window.CPO.editor.cm;
      cm.setCursor(cm.lastLine(), 0);
      cm.markText({line: 3, ch: 2}, {line: 3, ch: 7}, {className: "previous-run-highlight"});
      cm.foldCode(2, null, "fold");
    })()`);
    const body = "{line: 3, ch: 3}";
    const folded = `!!window.CPO.editor.cm.isFolded(${body})`;
    const highlighted = `window.CPO.editor.cm.findMarksAt(${body})
      .some(mark => mark.className === "previous-run-highlight")`;
    assert.equal(await page.eval(folded), true, "the function must start folded");
    assert.equal(await page.eval(highlighted), true, "the highlight must exist before Run");

    await page.eval("document.getElementById('runButton').click()");
    await waitOrFail(page, `window.PA.doneRendering() && ${IDLE}`, 60000, "Run did not finish");
    assert.equal((await page.eval("window.PA.shipshapeResult()")).shipshape, true);
    assert.equal(await page.eval(folded), true, "Run unfolded the function");
    assert.equal(await page.eval(highlighted), false, "Run left the old highlight in place");
  });
};

const assert = require("node:assert/strict");
const { test } = require("node:test");
const { evalAtReplNoError } = require("../shared/cpo-assertions");

module.exports = function registerNumber(getSession) {
  test("should render toggle-able numbers", { timeout: 80000 }, async () => {
    const page = getSession().page;
    await page.inject();
    await page.eval("window.PA.clearOutput()");
    await evalAtReplNoError(page, "1/7");
    await page.waitFor("document.getElementsByClassName('rationalRepeat').length > 0", 15000);
    const text = await page.eval("document.getElementsByClassName('rationalRepeat')[0].innerText");
    assert.strictEqual(text, "142857");
  });
};

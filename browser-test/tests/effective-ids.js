/*
 * effective-ids.js -- a long editor session must survive its own compiles.
 *
 * The TS compiler faithfully ported a recursive scan for an unused JS name
 * from anf-loop-compiler.arr, and the name set it scans grows with every
 * compile the page has done -- so around 90 compiles in, the scan blew the
 * JS stack. Running many distinct programs in a row keeps that caught; if it
 * comes back, the failing compile's output names it ("Maximum call stack
 * size exceeded").
 */
const { test } = require("node:test");
const { IDLE, runProgram, outputHas, waitOrFail } = require("./helpers");

const COMPILES = Number(process.env.PYRET_EFFECTIVE_IDS_COMPILES || 120);

module.exports = function registerEffectiveIds(getSession) {
  test("an editor that has compiled many programs still compiles the next one",
    { timeout: COMPILES * 6000 + 180000 }, async () => {
      const page = getSession().page;
      for (let i = 1; i <= COMPILES; i++) {
        await runProgram(page, i + " + 1\n");
        await waitOrFail(page, `${outputHas(String(i + 1))} && ${IDLE}`, 60000,
          "compile " + i + " of " + COMPILES + " did not produce " + (i + 1) +
          " on an idle editor");
      }
    });
};

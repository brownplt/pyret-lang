/*
 * stop-during-load.js -- Stop, pressed while the program's import is still
 * being fetched, must end the run right then -- not whenever the fetch
 * happens to finish -- and hand back a usable editor.
 *
 * There's also an assertion of an explicit regression: at one point the
 * continuation of the loading step could carry forward after a stop and still
 * run the program while the UI appeared stopped. The "no resurrection" rule
 * makes sure we don't hit that again.
 */
const assert = require("node:assert/strict");
const { test } = require("node:test");
const { ProceduralError } = require("../shared/errors");
const { IDLE, runProgram, outputText, outputHas, waitOrFail } = require("./helpers");

const HOLD_MS = 8000;
const RESURRECT_MS = 6000;

module.exports = function registerStopDuringLoad(getSession) {
  test("Stop pressed while an import is still loading ends the run during the load and leaves the editor usable",
    { timeout: 120000 }, async () => {
      const page = getSession().page;
      const run = await startRunWithHeldImport(page);
      await pressStopAtFirstChance(run);
      await expectStoppedDuringTheLoad(run);
      await expectTheReleaseToResurrectNothing(run);
      await expectANewRunToProduceResults(page);
    });
};

const IMPORTED_VALUE = "from-url-imports-lib";
const STOPPED_MESSAGE = "stopped by user";

const STOPPABLE =
  "window.replWidget.isRunning() === true && " +
  "(function(){var b=document.getElementById('breakButton');return !!(b && !b.disabled);})()";

const STOPPED_AND_IDLE = `${outputHas(STOPPED_MESSAGE)} && ${IDLE}`;

async function startRunWithHeldImport(page) {
  const base = process.env.PYRET_FIXTURE_BASE;
  if (!base) {
    throw new ProceduralError(
      "PYRET_FIXTURE_BASE is unset; run.js sets it when it starts the fixture server");
  }
  const url = base + "/pyret-programs/url-imports/lib/provided.arr?delay=" + HOLD_MS;
  await runProgram(page, 'import url("' + url + '") as S\n\nS.shared-value\n');
  return { page, releaseAt: Date.now() + HOLD_MS };
}

async function pressStopAtFirstChance(run) {
  try {
    await run.page.waitFor(STOPPABLE, Math.floor(HOLD_MS / 2));
  } catch (e) {
    throw new ProceduralError(
      "the break button never armed while the import was being held, so this " +
      "test never exercised its case");
  }
  await run.page.eval("document.getElementById('breakButton').click()");
}

// The deadline leaves the hold's last second unspent: meeting it proves the
// break landed while the fetch was still open, not once the load finished.
async function expectStoppedDuringTheLoad(run) {
  await waitOrFail(run.page, STOPPED_AND_IDLE, (run.releaseAt - 1000) - Date.now(),
    "Stop did not end the run as a user break while the import was still being fetched");
}

async function expectTheReleaseToResurrectNothing(run) {
  await new Promise((r) =>
    setTimeout(r, Math.max(0, run.releaseAt + RESURRECT_MS - Date.now())));
  const shown = await outputText(run.page);
  assert.ok(shown.indexOf(IMPORTED_VALUE) === -1,
    "the released fetch resurrected the stopped run. The editor shows: " +
    JSON.stringify(shown.slice(0, 200)));
}

async function expectANewRunToProduceResults(page) {
  await runProgram(page, "1 + 1\n");
  await waitOrFail(page, `${outputHas("2")} && ${IDLE}`, 30000,
    "after Stop, a later Run never produced its result on an idle editor");
}


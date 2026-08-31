/*
 * rapid-rerun.js -- two things outlive a finished run: the animation that
 * restores the interactions prompt, and the 1-second timer that arms the
 * Stop button. A run started inside that second must not be affected by
 * either leftover: the prompt stays hidden until the run really ends (a
 * visible prompt is how DOM readers decide a run is over), and a Stop
 * clicked the moment the button arms -- which is the FIRST run's timer
 * arming it -- cleanly breaks the live run.
 */
const assert = require("node:assert/strict");
const { test } = require("node:test");
const { IDLE, runProgram, outputHas, waitOrFail } = require("./helpers");

const FOREVER = "fun loop(n):\n  loop(n + 1)\nend\nloop(0)\n";

module.exports = function registerRapidRerun(getSession) {
  test("a run begun immediately after another is not affected by the first run's leftovers",
    { timeout: 120000 }, async () => {
      const page = getSession().page;

      await runProgram(page, "1 + 1\n");
      await waitOrFail(page, IDLE, 30000, "the warmup run never finished");

      await runProgram(page, FOREVER);
      const st = await stopAtFirstChance(page);
      assert.ok(st.claimed,
        "the editor stopped claiming a run that cannot have ended");
      assert.ok(!st.promptVisible,
        "the prompt is visible mid-run -- the previous run's leftover " +
        "animation resurrected it");

      await waitOrFail(page, `${outputHas("stopped by user")} && ${IDLE}`, 30000,
        "Stop did not end the run as a user break");

      await runProgram(page, "40 + 2\n");
      await waitOrFail(page, `${outputHas("42")} && ${IDLE}`, 30000,
        "a run after the Stop never produced its result on an idle editor");
    });
};

const BREAK_ARMED =
  "(function(){var b=document.getElementById('breakButton');return !!(b && !b.disabled);})()";

// Observe the claim and the prompt in the same in-page turn as the click,
// so break processing cannot muddy the readings.
const CHECK_AND_STOP = `(function(){
  var pc = document.querySelector(".prompt-container");
  var st = {
    claimed: window.replWidget.isRunning() === true,
    promptVisible: !pc || pc.offsetParent !== null
  };
  document.getElementById("breakButton").click();
  return st;
})()`;

async function stopAtFirstChance(page) {
  await waitOrFail(page, BREAK_ARMED, 10000,
    "the break button never armed during a non-terminating run");
  return page.eval(CHECK_AND_STOP);
}

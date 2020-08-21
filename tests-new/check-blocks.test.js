const assert = require('assert');
const glob = require('glob');
const fs = require('fs');
const path = require('path');
const cp = require('child_process');
const stream = require('stream');

const COMPILER_TIMEOUT = 10000; // ms, for each compiler run (including startup)
const RUN_TIMEOUT = 5000; // ms, for each program execution

const FAIL_SIGNAL = "Some tests failed";

describe("running runtime check blocks", () => {
  const files = glob.sync("build/runtime/*.arr.js", {});
  files.forEach(f => {
    test(`${f}`, () => {
      const runProcess = cp.spawnSync("node", [f], {stdio: "pipe", timeout: RUN_TIMEOUT});

      assert(runProcess.status === 0, `${runProcess.stdout}\n${runProcess.stderr}`);

      assert(runProcess.stdout.indexOf(FAIL_SIGNAL) === -1,
          `${runProcess.stdout} should not contain \"${FAIL_SIGNAL}\" and stderr was ${runProcess.stderr}`);

    });
  });
});

// TODO(alex): Duplicate and modified version of simple-output
describe("testing check block programs", () => {
  const files = glob.sync("tests-new/check-blocks/*.arr", {});
  files.forEach(f => {
    test(`${f}`, () => {
      let compileProcess;

      var typeCheck = f.match(/no-type-check/) === null;

      // Non-exact output check (scan):
      // Checks for any line beginning with prefix '###' and uses the trimmed content
      if (!typeCheck) {
        compileProcess = cp.spawnSync(
          "node",
          ["tests-new/run-pyret-no-type-check.js", f],
          {stdio: "pipe", timeout: COMPILER_TIMEOUT});
      } else {
        compileProcess = cp.spawnSync(
          "node",
          ["tests-new/run-pyret.js", f],
          {stdio: "pipe", timeout: COMPILER_TIMEOUT});
      }

          assert(compileProcess.status === 0, `${compileProcess.stdout}\n${compileProcess.stderr}`);

        const basename = path.basename(f);
        const dest = glob.sync(`./tests-new/.pyret/compiled/project/tests-new/check-blocks/**${basename}.js`)[0];

        const runProcess = cp.spawnSync("node", [dest], {stdio: 'pipe', timeout: RUN_TIMEOUT});
        assert(runProcess.status === 0, `${runProcess.stdout}\n${runProcess.stderr}`);

        assert(runProcess.stdout.indexOf(FAIL_SIGNAL) === -1,
          `${runProcess.stdout} should not contain \"${FAIL_SIGNAL}\" and stderr was ${runProcess.stderr}`);
    });
  });
});

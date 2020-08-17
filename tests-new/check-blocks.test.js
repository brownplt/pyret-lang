const assert = require('assert');
const glob = require('glob');
const fs = require('fs');
const path = require('path');
const cp = require('child_process');
const stream = require('stream');

const RUN_TIMEOUT = 5000; // ms, for each program execution

const FAIL_SIGNAL = "[FAIL]";

describe("running check blocks", () => {
  const files = glob.sync("build/runtime/*.arr.js", {});
  files.forEach(f => {
    test(`${f}`, () => {
      const runProcess = cp.spawnSync("node", [f], {stdio: "pipe", timeout: RUN_TIMEOUT});

      assert(runProcess.status === 0, `${runProcess.stdout}\n${runProcess.stderr}`);

      assert(runProcess.stdout.indexOf(FAIL_SIGNAL) === -1,
          `${runProcess.stdout} should not contain ${FAIL_SIGNAL} and stderr was ${runProcess.stderr}`);

    });
  });
});

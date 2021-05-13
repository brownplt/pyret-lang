const assert = require('assert');
const glob = require('glob');
const fs = require('fs');
const path = require('path');
const cp = require('child_process');
const stream = require('stream');

const COMPILER_TIMEOUT = 10000; // ms, for each compiler run (including startup)
const RUN_TIMEOUT = 5000; // ms, for each program execution

describe("testing simple-output programs", () => {
  let files;
  let dir;
  if(global._PYRET_PIPELINE === "ts-anchor") {
    dir = "ts-simple-output";
  }
  else {
    dir = "simple-output";
  }
  files = glob.sync(`tests-new/${dir}/*.arr`, {});
  files.forEach(f => {
    test(`${f}`, () => {
      let compileProcess;

      var typeCheck = f.match(/no-type-check/) === null;

      // Non-exact output check (scan):
      // Checks for any line beginning with prefix '###' and uses the trimmed content
      //   of the line after the prefix as expected search criteria in stdout
      var exact = f.match(/scan/) === null;
      var typecheck = "notypecheck";
      var pipeline = "anchor";
      if(typeCheck) { typecheck = "typecheck"; }
      if(global._PYRET_PIPELINE === "ts-anchor") { pipeline = "ts-anchor"; }
      compileProcess = cp.spawnSync(
        "node",
        ["tests-new/run-pyret.js", f, typecheck, pipeline],
        {stdio: "pipe", timeout: COMPILER_TIMEOUT});

      assert(compileProcess.status === 0, `${compileProcess.stdout}\n${compileProcess.stderr}`);

      const contents = String(fs.readFileSync(f));
      if (exact) {
        const firstLine = contents.split("\n")[0];
        const expect = firstLine.slice(firstLine.indexOf(" ")).trim();

        const basename = path.basename(f);
        const dest = glob.sync(`./tests-new/.pyret/compiled/project/tests-new/${dir}/**${basename}.js`)[0];

        const runProcess = cp.spawnSync("node", [dest], {stdio: 'pipe', timeout: RUN_TIMEOUT});
        assert(runProcess.status === 0, `${runProcess.stdout}\n${runProcess.stderr}`);
        assert(runProcess.stdout.indexOf(expect) !== -1, `${runProcess.stdout} should contain ${expect} and stderr was ${runProcess.stderr}`);

      } else {

        const lines = contents.split("\n");
        let expected = [];
        lines.forEach((line) => {
          if (line.startsWith("###")) {
            const formatted = line.slice(line.indexOf(" ")).trim();
            expected.push(formatted);
          }
        });

        const basename = path.basename(f);
        const dest = glob.sync(`./tests-new/.pyret/compiled/project/tests-new/${dir}/**${basename}.js`)[0];

        const runProcess = cp.spawnSync("node", [dest], {stdio: 'pipe', timeout: RUN_TIMEOUT});
        assert(runProcess.status === 0, `${runProcess.stdout}\n${runProcess.stderr}`);
        
        expected.forEach((expect) => {
          assert(runProcess.stdout.indexOf(expect) !== -1, `${runProcess.stdout} should contain ${expect} and stderr was ${runProcess.stderr}`);
        });
      }
    });
  });
});

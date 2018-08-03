const assert = require('assert');
const glob = require('glob');
const fs = require('fs');
const path = require('path');
const cp = require('child_process');

const COMPILER_TIMEOUT = 10000; // ms, for each compiler run (including startup)
const RUN_TIMEOUT = 5000; // ms, for each program execution

describe("testing simple-output programs", () => {
  const files = glob.sync("tests-new/simple-output/*.arr", {});
  files.forEach(f => {
    test(`${f}`, () => {

      const compileProcess = cp.spawnSync("node",
        [ "tests-new/run-pyret.js", f], {stdio: 'pipe', timeout: COMPILER_TIMEOUT});
        
      assert(compileProcess.status === 0, `${compileProcess.stdout}\n${compileProcess.stderr}`);

      const contents = String(fs.readFileSync(f));
      const firstLine = contents.split("\n")[0];
      const expect = firstLine.slice(firstLine.indexOf(" ")).trim();

      console.log(String(compileProcess.stdout), String(compileProcess.stderr));

      const basename = path.basename(f);
      var nested_path = "tests-new/.pyret/compiled/project/tests-new/simple-output/";
      var sofar = ".";
      for(piece of nested_path.split("/")) {
        console.log(sofar, fs.readdirSync(sofar));
        sofar = path.join(sofar, piece);
      }
      const dest = glob.sync(`./tests-new/.pyret/compiled/project/tests-new/simple-output/**${basename}.js`)[0];

      const runProcess = cp.spawnSync("node", [dest], {stdio: 'pipe', timeout: RUN_TIMEOUT});
      assert(runProcess.status === 0, `${runProcess.stdout}\n${runProcess.stderr}`);
      assert(runProcess.stdout.indexOf(expect) !== -1, `${runProcess.stdout} should contain ${expect}`);
    });
  });
});

const assert = require('assert');
const glob = require('glob');
const fs = require('fs');
const path = require('path');
const cp = require('child_process');

      const compileProcess = cp.spawnSync("node",
        [ "tests-new/run-pyret.js", "tests-new/simple-output/print1.arr"], {stdio: 'pipe'});

describe("testing simple-output programs", () => {
  const files = glob.sync("tests-new/simple-output/*.arr", {});
  files.forEach(f => {
    test(`${f}`, () => {

      const compileProcess = cp.spawnSync("node",
        [ "tests-new/run-pyret.js", f], {stdio: 'pipe'});
        
      assert(compileProcess.status === 0, `${compileProcess.stdout}\n${compileProcess.stderr}`);

      const contents = String(fs.readFileSync(f));
      const firstLine = contents.split("\n")[0];
      const expect = firstLine.slice(firstLine.indexOf(" ")).trim();

      const basename = path.basename(f);
      const dest = glob.sync(`./tests-new/.pyret/compiled/project/tests-new/simple-output/**${basename}.js`)[0];

      const runProcess = cp.spawnSync("node", [dest], {stdio: 'pipe'});
      assert(runProcess.status === 0, `${runProcess.stdout}\n${runProcess.stderr}`);
      assert(runProcess.stdout.indexOf(expect) !== -1, `${runProcess.stdout} should contain ${expect}`);
    });
  });
});

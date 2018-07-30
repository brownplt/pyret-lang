const assert = require('assert');
const glob = require('glob');
const fs = require('fs');
const path = require('path');
const cp = require('child_process');

describe("testing simple-output programs", () => {
  const files = glob.sync("tests/simple-output/*.arr", {});
  files.forEach(f => {
    test(`${f}`, () => {
      cp.spawnSync("rm", ["-rf", "tests/compiled"]);
      cp.spawnSync("mkdir", ["-p", "tests/compiled"]);
      const compileProcess = cp.spawnSync("node",
        [ "./build/phaseA/pyret.jarr"
        , "--builtin-js-dir"
        , "src/runtime"
        , "--compiled-dir"
        , "tests/compiled"
        , "--build-runnable"
        , f]);
      assert(compileProcess.status === 0, `${compileProcess.stdout}\n${compileProcess.stderr}`);

      const contents = String(fs.readFileSync(f));
      const firstLine = contents.split("\n")[0];
      const expect = firstLine.slice(firstLine.indexOf(" "));

      const basename = path.basename(f);
      const dest = glob.sync(`./tests/compiled/project/tests/simple-output/**${basename}.js`)[0];

      console.log("Dest", dest);

      const runProcess = cp.spawnSync("node", [dest], {stdio: 'pipe'});
      assert(runProcess.status === 0, `${runProcess.stdout}\n${runProcess.stderr}`);
      assert(runProcess.stdout.indexOf(expect) !== -1, `${runProcess.stdout} should contain ${expect}`);
    });
  });
});

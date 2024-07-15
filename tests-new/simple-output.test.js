const assert = require('assert');
const glob = require('glob');
const fs = require('fs');
const path = require('path');
const cp = require('child_process');
const stream = require('stream');

const COMPILER_TIMEOUT = 30000; // ms, for each compiler run (including startup)
const RUN_TIMEOUT = 20000; // ms, for each program execution

describe("testing simple-output programs", () => {
  let files;
  let dir;
  if(global._PYRET_PIPELINE && global._PYRET_PIPELINE.startsWith("ts-anchor")) {
    dir = "ts-simple-output";
  }
  else {
    dir = "simple-output";
  }
  files = glob.sync(`tests-new/${dir}/*.arr`, {});
  files = files.concat(glob.sync(`tests-new/${dir}/pyret-lang/*.arr`, {}));
  files.forEach(f => {
    test(`${f}`, () => {
      let compileProcess;

      var typeCheck = f.match(/no-type-check/) === null;

      var typecheck = "notypecheck";
      var pipeline = "anchor";
      if(typeCheck) { typecheck = "typecheck"; }
      if(global._PYRET_PIPELINE && 
         global._PYRET_PIPELINE.startsWith("ts-anchor")) { pipeline = global._PYRET_PIPELINE; }

      const contents = String(fs.readFileSync(f));
      const lines = contents.split("\n");
      let expected = [];
      let stderrExpected = [];
      lines.forEach((line) => {
        if (line.startsWith("###")) {
          const formatted = line.slice(line.indexOf(" ")).trim();
          expected.push(formatted);
        }
        if (line.startsWith("##!")) {
          const formatted = line.slice(line.indexOf(" ")).trim();
          stderrExpected.push(formatted);
        }
      });
      if(expected.length === 0 && stderrExpected.length === 0) {
        throw new Error("Test file did not define any expected output");
      }

      compileProcess = cp.spawnSync(
        "node",
        ["tests-new/run-pyret.js", f, typecheck, pipeline, "empty"],
        {stdio: "pipe", timeout: COMPILER_TIMEOUT});

      if(compileProcess.status !== 0) {
        let stderr = String(compileProcess.stderr);
        stderrExpected.forEach((expect) => {
          assert(stderr.indexOf(expect) !== -1, `=====EXPECTED STDERR TO CONTAIN THESE LINES IN ORDER=====\n${stderrExpected.join("\n")}\n=====ACTUAL STDERR=====\n${compileProcess.stderr} and stdout was\n${compileProcess.stdout}`);
          stderr = stderr.slice(stderr.indexOf(expect) + expect.length);
        });

        assert(stderrExpected.length !== 0, `Compilation failed, which was not expected (there were no expected errors): ${compileProcess.stdout}\n${compileProcess.stderr}`);
        return;
      }

      const basename = path.basename(f);
      const dest = glob.sync(`./tests-new/.pyret/compiled/project/tests-new/${dir}/**/${basename}.js`)[0];

      const runProcess = cp.spawnSync("node", [dest], {stdio: 'pipe', timeout: RUN_TIMEOUT});

      if(stderrExpected.length === 0) {
        assert(runProcess.status === 0, `${String(compileProcess.stdout)}\n${runProcess.stdout}\n${runProcess.stderr}`);
      }
      else {
        assert(runProcess.status !== 0, `${String(compileProcess.stdout)}\n${runProcess.stdout}\n${runProcess.stderr}`);
      }

      let stdout = runProcess.stdout;
      expected.forEach((expect) => {
        assert(stdout.indexOf(expect) !== -1, `=====EXPECTED STDOUT TO CONTAIN THESE LINES IN ORDER=====\n${expected.join("\n")}\n=====ACTUAL STDOUT=====\n${runProcess.stdout} and stderr was\n${runProcess.stderr}`);
        stdout = runProcess.stdout.slice(runProcess.stdout.indexOf(expect) + expect.length);
      });
      let stderr = runProcess.stderr;
      stderrExpected.forEach((expect) => {
        assert(stderr.indexOf(expect) !== -1, `=====EXPECTED STDERR TO CONTAIN THESE LINES IN ORDER=====\n${stderrExpected.join("\n")}\n=====ACTUAL STDERR=====\n${runProcess.stderr} and stdout was\n${runProcess.stdout}`);
        stderr = runProcess.stderr.slice(runProcess.stderr.indexOf(expect) + expect.length);
      });
    });
  });
});

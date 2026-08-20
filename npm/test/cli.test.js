const glob = require('glob');
const fs = require('fs');
const cp = require('child_process');
const assert = require('assert');

const COMPILER_TIMEOUT = 60000; // ms, for each compiler run (including startup)
const RUN_TIMEOUT = 60000; // ms, for each program execution
const SUCCESS_EXIT_CODE = 0;
const EMPTY_MESSAGE = "";

const parse_file_for_expected_std = (f) => {
  let stdioExpected = EMPTY_MESSAGE;
  let stdInToInject = EMPTY_MESSAGE;
  let stderrExpected = EMPTY_MESSAGE;
  let compilestderrExpected = EMPTY_MESSAGE;
  let extraArgs = [];

  String(fs.readFileSync(f))
    .split("\n")
    .forEach((line) => {
      // NOTE: we expect only one instance of each to be defined. However, if more
      // than one is defined, we will use the last one.
      
      // stdin
      if (line.startsWith("###<")) {
        stdInToInject = line.slice(line.indexOf(" ")).trim() + "\n";
      }
      
      // stdout
      if(line.startsWith("###>")) {
        stdioExpected = line.slice(line.indexOf(" ")).trim();
      }

      // stderr
      if(line.startsWith("###!")) {
        stderrExpected = line.slice(line.indexOf(" ")).trim();
      }

      if(line.startsWith("###@")) {
        extraArgs = line.slice(line.indexOf(" ")).trim().split(" ");
      }

      if(line.startsWith("###*")) {
        compilestderrExpected = line.slice(line.indexOf(" ")).trim();
      }
  });

  return {
    stdioExpected: stdioExpected,
    stdInToInject: stdInToInject,
    stderrExpected: stderrExpected,
    compilestderrExpected: compilestderrExpected,
    extraArgs: extraArgs
  }
}



const registerIOTests = (suiteName, backendArgs, testFn) => {
  describe(suiteName, () => {
    glob.sync(`test/tests/test-*.arr`, {}).forEach(f => {
      describe("Testing " + f, () => {
        const {stdioExpected, stdInToInject, stderrExpected, compilestderrExpected, extraArgs} = parse_file_for_expected_std(f);

        testFn(`it should return io that is expected: ${stdioExpected}`, () => {
          const args = [ "pyret.js", ...backendArgs, ...extraArgs, f ];
          const pyretProcess = cp.spawnSync(
            "node",
            args,
            {stdio: "pipe", stderr: "pipe", timeout: COMPILER_TIMEOUT});

          if(compilestderrExpected === "") {
            expect(pyretProcess.stderr.toString()).toEqual(EMPTY_MESSAGE);
            expect(pyretProcess.status).toEqual(SUCCESS_EXIT_CODE);
          }
          else {
            expect(pyretProcess.stderr.toString()).toContain(compilestderrExpected);
            expect(pyretProcess.status).not.toEqual(SUCCESS_EXIT_CODE);
            return; // Don't try to run the program if an error was expected
          }

          if (stderrExpected !== EMPTY_MESSAGE) {
            expect(pyretProcess.status).not.toEqual(SUCCESS_EXIT_CODE);
            expect(pyretProcess.stderr.toString()).toMatch(new RegExp(stderrExpected));
          }
          else {
            expect(pyretProcess.status).toEqual(SUCCESS_EXIT_CODE);
            expect(pyretProcess.stdout.toString()).toMatch(new RegExp(stdioExpected));
          }
        });
      });
    });
  });
};

registerIOTests("IO Tests", [], test);
registerIOTests(
  "IO Tests (ts backend)",
  ["--backend", "ts"],
  fs.existsSync("pyret-lang/build/ts-compiler/pyret.js") ? test : test.skip
);

const assert = require('assert');
const glob = require('glob');
const fs = require('fs');
const path = require('path');
const cp = require('child_process');
const stream = require('stream');
const tester = require("./test-util.js");

const TEST_TIMEOUT = 20000;
const COMPILER_TIMEOUT = 10000; // ms, for each compiler run
const STARTUP_TIMEOUT = 6000;

describe("Testing browser simple-output programs", () => {

  jest.setTimeout(TEST_TIMEOUT);

  let setup;
  let driver;
  let baseURL;
  let refreshPagePerTest;

  beforeAll(() => {
    setup = tester.setup();
    driver = setup.driver;
    baseURL = setup.baseURL;
    refreshPagePerTest = setup.refreshPagePerTest;

    if (refreshPagePerTest === false) {
      return driver.get(baseURL + "/page.html");
    }
  });

  beforeEach(() => {
    if (refreshPagePerTest === true) {
      return driver.get(baseURL + "/page.html");
    }
  });

  afterEach(() => {
    if (refreshPagePerTest === false) {
      return tester.clearLogs(driver);
    }
  });

  afterAll(() => { 
    return driver.quit();
  });

  describe("Basic page loads", function() {
    test("should load the webworker compiler", async function(done) {

      let loaded = await tester.pyretCompilerLoaded(driver, STARTUP_TIMEOUT);
      expect(loaded).toBeTruthy();

      // Testing program input works correctly
      let myValue = "FOO BAR";
      await tester.beginSetInputText(driver, myValue);

      let programInput = await driver.findElement({ id: "program" });
      let value = await programInput.getAttribute("value");

      expect(value).toEqual("FOO BAR");

      await done();
    });
  });

  describe("Testing simple output", function() {
    const files = glob.sync("tests-new/simple-output/*.arr", {});

    // According to beforeEach(), webdriver reopens the editor
    // This clears the console and resets the compiler
    // Used for simplicity's sake and in the event of mutating tests with overlapping variables
    // NOTE(alex): Reloading before each test is necessary because I can't find a source guarenteeing
    //   test run order beyond using test sequencers. Otherwise, define basic page loads to go first
    //   and execute simple output tests.
    //   Maybe use test sequencers in the future?
    files.forEach(f => {
      test(`${f}`, async function(done) {

        let typeCheck = f.match(/no-type-check/) === null;

        // Non-exact output check (scan):
        // Checks for any line beginning with prefix '###' and uses the trimmed content
        //   of the line after the prefix as expected search criteria in stdout
        let exact = f.match(/scan/) === null;

        if (refreshPagePerTest === true) {
          let loaded = await tester.pyretCompilerLoaded(driver, STARTUP_TIMEOUT);
          expect(loaded).toBeTruthy();
        }

        const contents = String(fs.readFileSync(f));

        if (exact) {
          const firstLine = contents.split("\n")[0];
          const expectedOutput = firstLine.slice(firstLine.indexOf(" ")).trim();

          await tester.beginSetInputText(driver, contents)
            .then(tester.compileRun(driver, { 
              'type-check': typeCheck,
              'stopify': false,
            }));

          // Does not work when in .then()
          let foundOutput = 
            await tester.searchForRunningOutput(driver, expectedOutput, COMPILER_TIMEOUT);

          let runtimeErrors = 
            await tester.areRuntimeErrors(driver);

          expect(foundOutput).toEqual(tester.OK);
          expect(runtimeErrors).toBeFalsy();

          await done();
        } else {

          const lines = contents.split("\n");
          let expected = [];
          lines.forEach((line) => {
            if (line.startsWith("###")) {
              const formatted = line.slice(line.indexOf(" ")).trim();
              expected.push(formatted);
            }
          });

          await tester.beginSetInputText(driver, contents)
            .then(tester.compileRun(driver, { 
              'type-check': typeCheck,
              'stopify': false,
            }));

          for (let i = 0; i < expected.length; i++) {
            const expectedOutput = expected[i];
            let foundOutput = 
              await tester.searchForRunningOutput(driver, 
                expectedOutput, COMPILER_TIMEOUT);

              expect({ expectedOutput, result: foundOutput })
                .toEqual({ expectedOutput, result: tester.OK });
          }
 
          let runtimeErrors = 
              await tester.areRuntimeErrors(driver);

          expect(runtimeErrors).toBeFalsy();

          await done();
        }
      });
    });
  });
});

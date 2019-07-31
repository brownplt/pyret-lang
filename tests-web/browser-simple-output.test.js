const assert = require('assert');
const glob = require('glob');
const fs = require('fs');
const path = require('path');
const cp = require('child_process');
const stream = require('stream');
const tester = require("./test-util.js");

const TEST_TIMEOUT = 20000;
const COMPILER_TIMEOUT = 10000; // ms, for each compiler run
const RUN_TIMEOUT = 5000; // ms, for each program execution
const STARTUP_TIMEOUT = 5000;

describe("Testing browser simple-output programs", () => {

  jest.setTimeout(TEST_TIMEOUT)

  describe("Basic page loads", function() {
    let setup = tester.setup();
    let driver = setup.driver;
    let baseURL = setup.baseURL;

    beforeAll(() => {
      return driver.get(baseURL + "/page.html");
    });

    afterAll(() => { 
      return driver.quit();
    });

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

  const files = glob.sync("tests-new/simple-output/*.arr", {});
  files.forEach(f => {
    test(`${f}`, async function(done) {

      let loaded = await tester.pyretCompilerLoaded(driver, STARTUP_TIMEOUT);
      expect(loaded).toBeTruthy();

      const contents = String(fs.readFileSync(f));
      const firstLine = contents.split("\n")[0];
      const expect = firstLine.slice(firstLine.indexOf(" ")).trim();

      await tester.beginSetInputText(driver, contents)
        .then(tester.compileRun(driver))
        .then(driver.sleep(COMPILER_TIMEOUT + RUN_TIMEOUT));

      let indexResult = await searchOutput(driver, new RegExp(contents));

      let result = indexResult !== -1;

      expect(result).toBeTruthy();

      await done();
    });
  });
});

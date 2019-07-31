const assert = require('assert');
const glob = require('glob');
const fs = require('fs');
const path = require('path');
const cp = require('child_process');
const stream = require('stream');
const tester = require("./test-util.js");

const TEST_TIMEOUT = 20000;
const COMPILER_TIMEOUT = 10000; // ms, for each compiler run (including startup)
const RUN_TIMEOUT = 5000; // ms, for each program execution

describe("Testing browser simple-output programs", () => {

  jest.setTimeout(TEST_TIMEOUT)

  describe("Basic page loads", function() {
    
    test("should load the webworker compiler", async function(done) {
      let setup = tester.setup();
      let driver = setup.driver;
      let baseURL = setup.baseURL;
      await driver.get(baseURL + "/page.html")

      let cl = await driver.findElement({ id: "consoleList" });

      let result = await driver.wait(async () => {
        let innerHTML = await cl.getAttribute("innerHTML");
        let index = innerHTML.search(/Worker setup done/);
        return index !== -1;
      }, 5000);

      expect(result).toBeTruthy();

      tester.teardown(driver, done);
    });

    test("should inject program text", async function(done) {
      let setup = tester.setup();
      let driver = setup.driver;
      let baseURL = setup.baseURL;
      
      await driver.get(baseURL + "/page.html")
      let loaded = await tester.pyretCompilerLoaded(driver);

      expect(loaded).toBeTruthy();

      await driver.executeScript(function() {
        document.getElementById("program").value = "FOO BAR";
      });

      let programInput = await driver.findElement({ id: "program" });
      let value = await programInput.getAttribute("value");

      expect(value).toEqual("FOO BAR");

      tester.teardown(driver, done);
    })
  });
});

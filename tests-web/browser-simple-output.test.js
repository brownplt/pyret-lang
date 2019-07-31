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

describe("testing simple-output programs", () => {

  jest.setTimeout(TEST_TIMEOUT)

  describe("Basic page loads", function() {
    
    test("should load the editor", async function(done) {
      let setup = tester.setup();
      let driver = setup.driver;
      let baseURL = setup.baseURL;
      await driver.get(baseURL + "/page.html")

      let cl = await driver.findElement({ id: "consoleList" });


      await driver.sleep(4000);
      let innerHTML = await cl.getAttribute("innerHTML");

      let result = innerHTML.search(/Worker setup done/);
     
      // let result = await tester.waitForPyretLoad(driver, 5000);
      expect(result != -1).toEqual(true)
      driver.quit().then(done);
    });
  });
});

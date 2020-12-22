import { strict as assert } from 'assert';
import { setup } from './util/setup';
import * as textMode from './util/TextMode';
// const glob = require('glob');
// const tester = require("./test-util.js");

const TEST_TIMEOUT = 20000;
const COMPILER_TIMEOUT = 10000; // ms, for each compiler run
const STARTUP_TIMEOUT = 6000;

describe("Testing browser simple-output programs", () => {

  jest.setTimeout(TEST_TIMEOUT);

  let driver;
  let ideURL;
  let refreshPagePerTest;

  beforeAll(() => {
    let setupResult = setup();
    driver = setupResult.driver;
    ideURL = setupResult.ideURL;
    refreshPagePerTest = setupResult.refreshPagePerTest;

    if (refreshPagePerTest === false) {
      return driver.get(ideURL);
    }
  });

  afterAll(() => {
    // return driver.quit();
  });

  test("Should load the IDE", async function(done) {

    await textMode.toTextMode(driver);
    await textMode.sendInput(driver, "FOO");

    await done();
  });
});

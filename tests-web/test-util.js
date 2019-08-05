var assert = require("assert");
var webdriver = require("selenium-webdriver");
var fs = require("fs");
const jsesc = require("jsesc");
const ffdriver = require('geckodriver');

let PATH_TO_FF;
// Used by Travis
if (process.env.FIREFOX_BINARY) {
  PATH_TO_FF = process.env.FIREFOX_BINARY;
}
else {
  throw "You can set FIREFOX_BINARY to the path to your Firefox install if this path isn't for your machine work";
}

let BASE_URL;
if (process.env.BASE_URL) {
  BASE_URL = process.env.BASE_URL;
} else {
  throw "Set BASE_URL to be the root of the compiler directory";
}

let leave_open = process.env.LEAVE_OPEN === "true" || false;

let args = [];

const ffCapabilities = webdriver.Capabilities.firefox();
ffCapabilities.set('moz:firefoxOptions', {
  binary: PATH_TO_FF,
  'args': args
});

const INPUT_ID = "program";
const COMPILE_RUN_BUTTON = "compileRun";

function setup() {
  let driver = new webdriver.Builder()
    .forBrowser("firefox")
    .setProxy(null)
    .withCapabilities(ffCapabilities).build();

  return {
    driver: driver,
    baseURL: BASE_URL,
  };
}

function beginSetInputText(driver, unescapedInput) {
  // TODO(alex): Find a way to properly escape
  let escapedInput = jsesc(unescapedInput, {quotes: "double"});
  return driver.executeScript(
    "document.getElementById(\"" + INPUT_ID + "\").value = \"" + escapedInput + "\";"
  );
}

async function compileRun(driver) {
  let runButton = await driver.findElement({ id: COMPILE_RUN_BUTTON });
  await runButton.click();
}

async function pyretCompilerLoaded(driver, timeout) {
  let cl = await driver.findElement({ id: "consoleList" });

  let result = await driver.wait(async () => {
    let innerHTML = await cl.getAttribute("innerHTML");
    let index = innerHTML.search(/Worker setup done/);
    return index !== -1;
  }, timeout);

  return result;
}

async function searchForRunningOutput(driver, toSearch, timeout) {
  let cl = await driver.findElement({ id: "consoleList" });

  try {
    let result = await driver.wait(async () => {
      let innerHTML = await cl.getAttribute("innerHTML");
      let runningIndex = innerHTML.search(/Running/);
      
      if (runningIndex !== -1) {
        let toSearchIndex = innerHTML.substring(runningIndex).includes(toSearch);
        return toSearchIndex !== -1;
      } else {
        return false;
      }
    }, timeout);

    return result === null ? false : result;
  } catch (error) {
    return false;
  }
};

async function searchOutput(driver, pattern) {
  let cl = await driver.findElement({ id: "consoleList" });
  let innerHTML = await cl.getAttribute("innerHTML");

  // -1 if pattern not found
  return innerHTML.search(pattern);
}

function prepareExpectedOutput(rawExpectedOutput) {
  // TODO(alex): Remove leading '###' and trim
  return rawExpectedOutput;
}

function teardown(browser, done) {
  if(!leave_open) {
    return browser.quit().then(done);
  }

  return done;
}

module.exports = {
  pyretCompilerLoaded: pyretCompilerLoaded,
  setup: setup,
  teardown: teardown,
  beginSetInputText: beginSetInputText,
  compileRun: compileRun,
  searchOutput: searchOutput,
  searchForRunningOutput: searchForRunningOutput,
};

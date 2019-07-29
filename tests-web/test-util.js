var assert = require("assert");
var webdriver = require("selenium-webdriver");
var fs = require("fs");
const ffdriver = require('geckodriver');

let PATH_TO_FF;
// Used by Travis
if (process.env.FIREFOX_BINARY) {
  PATH_TO_FF = process.env.FIREFOX_BINARY;
}
else {
  throw "You can set FIREFOX_BINARY to the path to your Firefox install if this path isn't for your machine work";
}

let leave_open = process.env.LEAVE_OPEN === "true" || false;

let args = [];

const ffCapabilities = webdriver.Capabilities.firefox();
ffCapabilities.set('moz:firefoxOptions', {
  binary: PATH_TO_FF,
  'args': args
});

function setup() {
  setupWithName.call(this, undefined)
}

function setupWithName(name) {
  if(this.currentTest) { name = this.currentTest.title; }
  this.base = process.env.BASE_URL;
  this.browser = new webdriver.Builder()
    .forBrowser("firefox")
    .withCapabilities(ffCapabilities).build();

//  this.browser.manage().window().maximize();

  return;
}

function pyretLoaded(driver) {
  return driver.findElement(webdriver.By.id("consoleList"))
    .then(function(el) { return el.search(/Worker setup done/); });
}

function waitForPyretLoad(driver, timeout) {
  return driver.wait(function() { return pyretLoaded(driver); }, timeout);
}

function prepareExpectedOutput(rawExpectedOutput) {
  // TODO(alex): Remove leading '###' and trim
  return rawExpectedOutput;
}

function teardown() {
  if(!(this.currentTest.state === 'failed' || leave_open)) {
    return this.browser.quit();
  }
}

module.exports = {
  pyretLoaded: pyretLoaded,
  waitForPyretLoad: waitForPyretLoad,
  setup: setup,
  setupWithName: setupWithName,
  teardown: teardown,
};

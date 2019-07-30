var assert = require("assert");
var tester = require("../test-util.js");
var webdriver = require("selenium-webdriver");

function test() {
  let driver = tester.setup();
  let base = process.env.BASE_URL;

/*
  it("should load the index", function(done) {
    this.timeout(10000);
    this.driver.get(this.base);
    var headline = this.driver.findElement(webdriver.By.id('right'));
    this.driver.call(done);
  });
*/

  console.log(base);
  driver.get(base + "/page.html");
  let result = tester.pyretLoaded(driver);
  console.log("Result: " + result);
  if (result === false) {
    throw "\"Worker setup done\" message not ofund";
  }

  tester.teardown(driver);
}

test();

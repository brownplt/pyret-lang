const webdriver = require("selenium-webdriver");
const seleniumChrome = require("selenium-webdriver/chrome");
const ffdriver = require('geckodriver');
const chromedriver = require('chromedriver');

enum BrowserKind {
  Chrome,
  Firefox,
}

// Get the browser binary path and detect the browser kind
let BROWSER: string;
let BROWSER_KIND: BrowserKind;
if (process.env.BROWSER) {
  BROWSER = process.env.BROWSER;
  if (BROWSER.includes("chrome")) {
    BROWSER_KIND = BrowserKind.Chrome;
  }

  if (BROWSER.includes("firefox")) {
    BROWSER_KIND = BrowserKind.Firefox;
  }

  if (BROWSER_KIND === undefined) {
    throw `Unknown browser kind: ${BROWSER}.`
  }
} else {
  throw `Set $BROWSER to the browser binary path`;
}

let PORT: string;
if (process.env.PORT) {
  PORT = process.env.PORT;
} else {
  throw `Set $PORT to the IDE http-server port`;
}

// Get the URL for the ide
let IDE_URL: string;
if (process.env.IDE_URL) {
  IDE_URL = process.env.IDE_URL;
} else {
  throw `Set $IDE_URL to the IDE URL`;
}

const BASE_URL = IDE_URL + ":" + PORT;

let refreshPagePerTest = true;
if (process.env.BROWSER_TEST_REFRESH) {
  refreshPagePerTest = (process.env.BROWSER_TEST_REFRESH === "true");
}

let chromeOptions = new seleniumChrome
  .Options();
if (process.env.SHOW_BROWSER === "false") {
  chromeOptions = chromeOptions.headless();
  console.log("Running headless (may not work with Firefox). You can set SHOW_BROWSER=true to see what's going on");

}

let args = [];
const ffCapabilities = webdriver.Capabilities.firefox();
ffCapabilities.set('moz:firefoxOptions', {
  binary: BROWSER,
  'args': args
});

// Working from https://developers.google.com/web/updates/2017/04/headless-chrome#drivers
const chromeCapabilities = webdriver.Capabilities.chrome();
chromeCapabilities.set('chromeOptions', {
  binary: BROWSER,
});

const INPUT_ID = "program";
const COMPILE_RUN_BUTTON = "compileRun";
const COMPILE_RUN_STOPIFY_BUTTON = "compileRunStopify";
const TYPE_CHECK_CHECKBOX = "typeCheck";
const CLEAR_LOGS = "clearLogs";

export function setup() {

  let driver;
  if (BROWSER_KIND === BrowserKind.Firefox) {
    driver = new webdriver.Builder()
      .forBrowser("firefox")
      .withCapabilities(ffCapabilities)
      .build();
  } else if (BROWSER_KIND === BrowserKind.Chrome) {
    driver = new webdriver.Builder()
      .forBrowser("chrome")
      .setChromeOptions(chromeOptions)
      .withCapabilities(chromeCapabilities)
      .build();
  } else {
    throw `Unknown browser: ${BROWSER}`;
  }

  return {
    driver: driver,
    ideURL: BASE_URL,
    refreshPagePerTest: refreshPagePerTest,
  };
}

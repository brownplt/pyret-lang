const assert = require("node:assert/strict");
const { test } = require("node:test");

const ENV = process.env.PYRET_ENV;
const SKIP = ENV !== "cpo" && "static page routes require the CPO server (cpo env)";
const BASE_URL = (process.env.BASE_URL || "http://localhost:4999").replace(/\/+$/, "");

const STATIC_PAGES = [
  "/faq",
];

module.exports = function registerStaticPages(getSession) {
  for (const endpoint of STATIC_PAGES) {
    test(endpoint + " substitutes APP_NAME and APP_DOMAIN", { timeout: 20000, skip: SKIP }, async () => {
      const browser = getSession().browser;
      const context = await browser.newContext();
      try {
        const page = await context.newPage();
        const resp = await page.goto(BASE_URL + endpoint, { waitUntil: "domcontentloaded", timeout: 20000 });
        assert.ok(resp && resp.ok(), endpoint + " returned HTTP " + (resp ? resp.status() : "no response"));

        const title = await page.title();
        assert.ok(
          !title.includes("{{APP_NAME}}"),
          endpoint + " title should not contain unsubstituted {{APP_NAME}}, got: " + title
        );
        assert.ok(title.length > 0, endpoint + " title should not be empty");

        const bodyText = await page.evaluate(() => document.body.innerText);
        assert.ok(!bodyText.includes("{{APP_NAME}}"), endpoint + " body should not contain unsubstituted {{APP_NAME}}");
        assert.ok(!bodyText.includes("{{APP_DOMAIN}}"), endpoint + " body should not contain unsubstituted {{APP_DOMAIN}}");
        assert.ok(!bodyText.includes("{{"), endpoint + " body should not contain any unsubstituted Mustache tokens");

        const appName = process.env.APP_NAME || "code.pyret.org";
        assert.ok(
          bodyText.includes(appName),
          endpoint + " body should contain APP_NAME '" + appName + "'"
        );
      } finally {
        await context.close();
      }
    });
  }
};

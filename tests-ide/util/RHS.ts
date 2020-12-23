import * as Selenium from 'selenium-webdriver';
import {
  WebElement,
  By
} from 'selenium-webdriver';

const RHS_CLASS: string = "interactions-area-container";

export async function searchFor(driver, input: string, strict?: boolean) {
  let rhs = await driver.findElement(By.className(RHS_CLASS));
  let results = await rhs.findElements(By.css("pre"));

  for (let i = 0; i < results.length; i++) {
    let inner = await results[i].getAttribute("innerHTML");
    if (strict) {
      if (inner === input) {
        return true;
      }
    } else {
      if (inner.includes(input)) {
        return true;
      }
    }
  }

  return false;
}

import * as Selenium from 'selenium-webdriver';
import {
  WebElement,
  By
} from 'selenium-webdriver';

const OPTION_BUTTON_ID = 'optionsButton';
const TEXT_MODE_BUTTON_ID = 'textModeButton';

/// Assumes already in Text Mode
export async function appendInput(driver, input) {

  let codeMirror = await driver.findElement(By.className("CodeMirror"));
  let codeLines = await codeMirror.findElements(By.className("CodeMirror-line"));
  let codeLine = codeLines.pop();
  await codeLine.click();

  let inputArea = await codeMirror.findElement(By.css("textarea"));

  await inputArea.sendKeys(input);
}

export async function toTextMode(driver) {
  // Open the 'Options' menu
  let optionsButton = await driver.findElement(By.id(OPTION_BUTTON_ID));
  await optionsButton.click();

  // Change to 'Text Mode'
  let textModeButton = await driver.findElement(By.id(TEXT_MODE_BUTTON_ID));
  await textModeButton.click();

  // Close the 'Options' menu
  await optionsButton.click();
}

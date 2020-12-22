const selenium = require("selenium-webdriver");

const OPTION_BUTTON_ID = 'optionsButton';
const TEXT_MODE_BUTTON_ID = 'textModeButton';
const CODE_MIRROR_INPUT_CLASS = 'CodeMirror';

const By = selenium.By;

/// Assumes already in Text Mode
export async function sendInput(driver, input) {

  // CodeMirror stores code in div elements of class CODE_MIRROR_INPUT_CLASS
  // In text mode, should only be one `CodeMirror-code` element
  // let inputArea = await driver.findElement({ className: CODE_MIRROR_INPUT_CLASS });
  console.log("FOO");

  // let codeMirror = await driver.findElement(By.className("CodeMirror-scroll"));
  let codeMirror = await driver.findElement(By.css('.CodeMirror textarea'));
  //console.log("BAZ");
  //await codeMirror.click();
  //console.log("QUX");
  console.log(await codeMirror.getAttribute("visible"));
  await codeMirror.sendKeys("Hello World");

  //let value = await inputArea.getAttribute("class");
  //console.log(value);

  // await inputArea.sendKeys(input);
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

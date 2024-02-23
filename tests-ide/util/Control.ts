import * as Selenium from 'selenium-webdriver';
import {
  WebElement,
  By
} from 'selenium-webdriver';


export async function manualCompile(driver) {
  let compileButton = await driver.findElement(By.id("CompileButton"));
  await compileButton.click();
}

export async function manualRun(driver) {
  let runButton = await driver.findElement(By.id("RunButton"));
  await runButton.click();
}

export async function stopExecution(driver) {
  let stopButton = await driver.findElement(By.id("StopButton"));
  await stopButton.click();
}

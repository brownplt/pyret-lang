import * as Selenium from 'selenium-webdriver';
import {
  WebElement,
  By
} from 'selenium-webdriver';

export enum EditorResponseLoop {
  Manual,
  AutoCompile,
  AutoCompileRun,
}

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

export async function changeEditorLoop(driver, erl: EditorResponseLoop) {
  let controlOptionsButton = await getRunOptionsButton(driver);

  await controlOptionsButton.click();

  let editorResponseButton =
    await controlOptionsButton.findElement(By.id("OptionEditorResponseLoop"));

  await editorResponseButton.click();

  let toSet;
  switch(erl) {
    case EditorResponseLoop.Manual:
      toSet = "OptionERLManual";
      break;

  case EditorResponseLoop.AutoCompile:
      toSet = "OptionERLAutoCompile";
      break;

  case EditorResponseLoop.AutoCompileRun:
      toSet = "OptionERLAutoCompileRun";
      break;
  }

  let toSetButton = await editorResponseButton.findElement(By.id(toSet));
  await toSetButton.click();
}

async function getRunOptionsButton(driver) {
  let button =  await driver.findElement(By.className("run-options"));
  return button;
}

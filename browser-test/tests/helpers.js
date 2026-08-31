/*
 * helpers.js -- the vocabulary the editor state-machine tests share: put a
 * program in the editor and run it, read the screen, and wait for a fact
 * with a failure that says what was expected and what is shown.
 */
const assert = require("node:assert/strict");
const { ProceduralError } = require("../shared/errors");

// Idle exactly as repl-ui.js's afterRun leaves the editor: `running` cleared
// and the interactions prompt back.
const IDLE =
  "window.replWidget.isRunning() !== true && " +
  "(function(){var pc=document.querySelector('.prompt-container');" +
  "return !pc || pc.offsetParent !== null;})()";

async function install(page, code) {
  for (let i = 0; i < 20; i++) {
    await page.eval("window.PA.setDefinitions(" + JSON.stringify(code) + ")");
    if ((await page.eval("window.PA.cmValue()")) === code) return;
    await new Promise((r) => setTimeout(r, 100));
  }
  throw new ProceduralError("could not install the program into the editor (doc-sync race)");
}

async function runProgram(page, code) {
  await install(page, code);
  await page.eval("window.PA.clearOutput()");
  await page.eval("window.PA.run()");
}

async function outputText(page) {
  return (await page.eval("window.PA.outputText()")).replace(/\s+/g, " ").trim();
}

function outputHas(text) {
  return `window.PA.outputText().indexOf(${JSON.stringify(text)}) !== -1`;
}

async function waitOrFail(page, expr, ms, message) {
  try {
    await page.waitFor(expr, Math.max(1, ms));
  } catch (e) {
    assert.fail(message + ". The editor shows: " +
      JSON.stringify((await outputText(page)).slice(0, 200)));
  }
}

module.exports = { IDLE, install, runProgram, outputText, outputHas, waitOrFail };

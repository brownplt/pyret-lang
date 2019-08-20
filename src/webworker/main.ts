import * as control from './control';

control.installFileSystem();

const consoleSetup = require("./console-setup.ts");

const input = <HTMLInputElement>document.getElementById("program");
const compile = document.getElementById("compile");
const compileRun = document.getElementById("compileRun");
const compileRunStopify = document.getElementById("compileRunStopify");
const typeCheckBox = <HTMLInputElement>document.getElementById("typeCheck");

const showBFS = <HTMLInputElement>document.getElementById("showBFS");

const FilesystemBrowser = require("./filesystemBrowser.ts");
const filesystemBrowser = document.getElementById('filesystemBrowser');
FilesystemBrowser.createBrowser(control.bfsSetup.fs, "/", filesystemBrowser);

const NO_RUNS = "none";
var runChoice = NO_RUNS;

const clearFSButton = document.getElementById("clearFS");
clearFSButton.onclick = function() {
  control.removeRootDirectory();
}

function loadBuiltins() {
  console.log("LOADING RUNTIME FILES");
  control.loadBuiltins();
  console.log("FINISHED LOADING RUNTIME FILES");
}

const loadBuiltinsButton = document.getElementById("loadBuiltins");
loadBuiltinsButton.onclick = function() {
  loadBuiltins();
}

loadBuiltins();

function compileHelper() {
  control.bfsSetup.fs.writeFileSync("./projects/program.arr", input.value);
  var typeCheck = typeCheckBox.checked;

  control.compile(
    control.path.compileBase,
    control.path.compileProgram ,
    typeCheck);
}

compile.onclick = compileHelper;

compileRun.onclick = function() {
  compileHelper();
  runChoice = 'SYNC';
};

compileRunStopify.onclick = function() {
  compileHelper();
  runChoice = 'ASYNC';
};

function echoLog(contents) {
  consoleSetup.workerLog(contents);
}

function echoErr(contents) {
  consoleSetup.workerError(contents);
}

function compileFailure() {
  consoleSetup.workerError("Compilation failure");
}

function compileSuccess() {
  console.log("Compilation succeeded!");

  if (runChoice !== NO_RUNS) {
    console.log("Running...");
    control.run(
      "/compiled/project",
      "program.arr.js",
      (result) => {
        console.log("Run complete with: ", result.result);
        console.log("Run complete in: ", result.time);
      },
      runChoice === 'ASYNC' ? control.backend.RunKind.Async : control.backend.RunKind.Sync);
    runChoice = NO_RUNS;
  }
}

const backendMessageHandler = control.backend.makeBackendMessageHandler(echoLog, echoErr, compileFailure, compileSuccess);

control.worker.onmessage = function(e) {

  // Handle BrowserFS messages
  if (e.data.browserfsMessage === true && showBFS.checked === false) {
    return;
  }

  try {
    var msgObject = JSON.parse(e.data);

    // Handle BrowserFS messages
    try {
      let innerData = JSON.parse(msgObject.data);
      if (innerData.browserfsMessage === true && showBFS.checked === false) {
        return;
      }

    } catch(error) { }

    var tag = msgObject["tag"];
    if (tag !== undefined) {
      if (tag === "log") {
        consoleSetup.workerLog(msgObject.data);
      } else if (tag === "error") {
        consoleSetup.workerError(msgObject.data);
      } else {
        consoleSetup.workerLog(msgObject.data);
      }
    } else {
      if (backendMessageHandler(e) === null) {
        consoleSetup.workerLog("FALLEN THROUGH:", e.data);
      }
    }
  } catch(error) {
    consoleSetup.workerLog("Error occurred: ", error, e.data);
  }
};

const setup = require("./setup.ts");
const runner = require("./runner.ts");
const pyretApi = require("./pyret-api.ts");

const fs = setup.BrowserFS.BFSRequire("fs");
const worker = setup.worker;

const input = <HTMLInputElement>document.getElementById("program");
const compile = document.getElementById("compile");
const compileRun = document.getElementById("compileRun");
const compileRunStopify = document.getElementById("compileRunStopify");

const showBFS = <HTMLInputElement>document.getElementById("showBFS");

var runChoice = 'none';

function compileProgram() {
  fs.writeFileSync("./projects/program.arr", input.value);
  let message = {
    _parley: true,
    options: {
      program: "program.arr",
      "base-dir": "/projects",
      "builtin-js-dir": "/prewritten/",
      checks: "none",
    }
  };
  worker.postMessage(message);
}

compile.onclick = compileProgram;

compileRun.onclick = function() {
  compileProgram();
  runChoice = 'sync';
};
compileRunStopify.onclick = function() {
  compileProgram();
  runChoice = 'async';
};

worker.onmessage = function(e) {

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
        setup.workerLog(msgObject.data);
      } else if (tag === "error") {
        setup.workerError(msgObject.data);
      } else {
        setup.workerLog(msgObject.data);
      }
    } else {
      var msgType = msgObject["type"];
      if (msgType == "echo-log") {
        setup.workerLog(msgObject.contents);
      } else if (msgType == "echo-err") {
        setup.workerError(msgObject.contents);
      } else if (msgType == "compile-failure") {
        setup.workerError("Compilation failure");
      } else if (msgType == "compile-success") {
        console.log("Compilation succeeded!");
        const start = window.performance.now();
        function printTimes() {
          const end = window.performance.now();
          console.log("Running took: ", end - start);
        }
        if (runChoice === 'sync') {
          runChoice = 'none';
          console.log("Running...");
          const start = window.performance.now();
          const result = runner.makeRequire("/compiled/project")("program.arr.js");
          const end = window.performance.now();
          console.log("Run complete with: ", result);
          printTimes();
        }
        else if (runChoice === 'async') {
          runChoice = 'none';
          const entry = runner.makeRequireAsync("/compiled/project");
          const resultP = entry("program.arr.js");
          resultP.catch((e) => { console.log("Run failed with: ", e); printTimes(); });
          resultP.then((r) => { console.log("Run complete with: " , r); printTimes(); });
        }
      } else {
        setup.workerLog(e.data);
      }

    }
  } catch(error) {
    setup.workerLog("Error occurred: ", error, e.data);
  }
};

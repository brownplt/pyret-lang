const setup = require("./setup.ts");
const runner = require("./runner.ts");
const pyretApi = require("./pyret-api.ts");

const fs = setup.BrowserFS.BFSRequire("fs");
const worker = setup.worker;

const input = <HTMLInputElement>document.getElementById("program");
const compile = document.getElementById("compile");
const compileRun = document.getElementById("compileRun");

const showBFS = <HTMLInputElement>document.getElementById("showBFS");

var shouldRun = false;

function compileProgram() {
  fs.writeFileSync("./projects/program.arr", input.value);
  let message = {
    _parley: true,
    options: {
      program: "program.arr",
      "base-dir": "/projects",
      "builtin-js-dirs": ["/prewritten/", "/uncompiled/"],
      checks: "none",
    }
  };
  worker.postMessage(message);
}

compile.onclick = compileProgram;

compileRun.onclick = function() {
  compileProgram();
  shouldRun = true;
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
        if (shouldRun) {
          console.log("Running...");
          runner.makeRequire("/compiled/project")("program.arr.js");
          shouldRun = false;
        }
      } else {
        setup.workerLog(e.data);
      }

    }
  } catch(error) {
    setup.workerLog(e.data);
  }
};

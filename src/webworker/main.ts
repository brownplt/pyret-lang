const setup = require("./setup.ts");
const runner = require("./runner.ts");
const pyretApi = require("./pyret-api.ts");

const fs = setup.BrowserFS.BFSRequire("fs");
const worker = setup.worker;

const input = <HTMLInputElement>document.getElementById("program");
const compile = document.getElementById("compile");

// Setup HTML output
// NOTE(alex): May need to CTRL + F5 (refresh and clear cache) in order to see HTML logs
var consoleOutputElement = document.getElementById("consoleOut");
var oldLog = console.log;
var consoleOutput = "";
console.log = function(message) {
  var outputLine = "[LOG]"; 
  for (let i = 0; i < arguments.length; i++) {
    var separator = ",";
    if (i === arguments.length - 1) {
      separator = "";
    }

    var arg = arguments[i];
    if (typeof arg === "object") {
      outputLine += JSON.stringify(arg, null, 4) + separator;
    } else {
      outputLine += arg + separator;
    }
  }
  outputLine += "\n";
  
  consoleOutput += outputLine;

  consoleOutputElement.innerHTML = consoleOutput;
  oldLog(message, arguments);
};

compile.onclick = function() {
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
  console.log('Message posted to worker');
};

worker.onmessage = function(e) {
  console.log("Message from worker: ", e);
};

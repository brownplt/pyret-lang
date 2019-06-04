const BrowserFS = require("browserfs");

const myWorker = new Worker('pyret.jarr');

window["projectsDir"] = "./projects";

// How to use BrowserFS with Web Workers: https://github.com/jvilk/BrowserFS/issues/210
BrowserFS.install(window);
BrowserFS.configure({
  fs: "LocalStorage"
}, function(e) {
  BrowserFS.FileSystem.WorkerFS.attachRemoteListener(myWorker);

  let fs = BrowserFS.BFSRequire("fs");
  if (fs.existsSync(window["projectsDir"]) === false) {
    fs.mkdirSync(window["projectsDir"]);
  }

  if (e) {
    throw e;
  }
});

window["BrowserFS"] = BrowserFS;
module.exports = {
  BrowserFS: BrowserFS,
  worker: myWorker
};

// Setup HTML output
// NOTE(alex): May need to CTRL + F5 (refresh and clear cache) in order to see HTML logs
var consoleOutputElement = document.getElementById("consoleOut");
var oldLog = console.log;
var consoleOutput = "";

var genericLog = function(prefix, ...args: any[]) {
  var outputLine = prefix;
  let logArgs = arguments[1];
  for (let i = 0; i < logArgs.length; i++) {
    var separator = ",";
    if (i === logArgs.length - 1) {
      separator = "";
    }

    var arg = logArgs[i];
    if (typeof arg === "object") {
      outputLine += JSON.stringify(arg, null, 4) + separator;
    } else {
      outputLine += arg + separator;
    }
  }
  outputLine += "\n";
  
  consoleOutput += outputLine;

  consoleOutputElement.innerHTML = consoleOutput;
};

console.log = function(message) {
  genericLog("[LOG]", arguments);
  oldLog(message, arguments);
}

var workerLog = function(message) {
  genericLog("[WORKER]", arguments);
  oldLog(message, arguments);
};

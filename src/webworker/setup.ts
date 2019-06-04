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

// Setup HTML output
// NOTE(alex): May need to CTRL + F5 (refresh and clear cache) in order to see HTML logs
var consoleOutputElement = document.getElementById("consoleOut");
var consoleOutput = "";
const oldLog = console.log;

const genericLog = function(prefix, ...args: any[]) {
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

console.log = function() {
  genericLog("[LOG]", arguments);
  oldLog.apply(console, arguments);
}

const workerLog = function() {
  genericLog("[WORKER]", arguments);
  oldLog.apply(console, arguments);
};

window["BrowserFS"] = BrowserFS;
module.exports = {
  BrowserFS: BrowserFS,
  worker: myWorker,
  workerLog: workerLog
};

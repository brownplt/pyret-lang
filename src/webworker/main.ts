const myWorker = new Worker("pyret.jarr");
const projectsDir = "/projects";
const prewritten = "/prewritten";
const uncompiled = "/uncompiled";

const consoleSetup = require("./console-setup.ts");
const bfsSetup = require("./browserfs-setup.ts")(myWorker, projectsDir);
const BrowserFS = bfsSetup.BrowserFS;

const backend = require("./backend.ts");

const runner = require("./runner.ts");
const pyretApi = require("./pyret-api.ts");

const loader = require("./runtime-loader.ts");
const fs = bfsSetup.BrowserFS.BFSRequire("fs");
const worker = bfsSetup.worker;

const input = <HTMLInputElement>document.getElementById("program");
const compile = document.getElementById("compile");
const compileRun = document.getElementById("compileRun");
const compileRunStopify = document.getElementById("compileRunStopify");
const typeCheckBox = <HTMLInputElement>document.getElementById("typeCheck");

const showBFS = <HTMLInputElement>document.getElementById("showBFS");

const FilesystemBrowser = require("./filesystemBrowser.ts");
const filesystemBrowser = document.getElementById('filesystemBrowser');
FilesystemBrowser.createBrowser(fs, "/", filesystemBrowser);

const myProgram = "program.arr";
const baseDir = "/projects";
const builtinJSDir = "/prewritten/";
const checks = "none";

const thePath = bfsSetup.BrowserFS.BFSRequire("path");

function deleteDir(dir) {
  // console.log("Entering:", dir);
  fs.readdir(dir, function(err, files) {
    if (err) {
      throw err;
    }

    let count = files.length;
    files.forEach(function(file) {
      let filePath = thePath.join(dir, file);
      
      fs.stat(filePath, function(err, stats) {
        if (err) {
          throw err;
        }

        if (stats.isDirectory()) {
          deleteDir(filePath);
        } else {
          fs.unlink(filePath, function(err) {
            if (err) {
              throw err;
            }

            console.log("Deleted:", filePath);
          });
        }
      });
    });
  });
}

const clearFSButton = document.getElementById("clearFS");
clearFSButton.onclick = function() {
  deleteDir("/");
}

function loadBuiltins() {
  console.log("LOADING RUNTIME FILES");
  loader(BrowserFS, prewritten, uncompiled);
  console.log("FINISHED LOADING RUNTIME FILES");
}

const loadBuiltinsButton = document.getElementById("loadBuiltins");
loadBuiltinsButton.onclick = function() {
  loadBuiltins();
}

loadBuiltins();

var runChoice = 'none';

function compileHelper() {
  fs.writeFileSync("./projects/program.arr", input.value);
  var typeCheck = typeCheckBox.checked;

  backend.compileProgram(myWorker, {
    program: myProgram,
    baseDir: baseDir,
    builtinJSDir: builtinJSDir,
    checks: checks,
    typeCheck: typeCheck,
  });
}

compile.onclick = compileHelper;

compileRun.onclick = function() {
  compileHelper();
  runChoice = 'sync';
};

compileRunStopify.onclick = function() {
  compileHelper();
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
        consoleSetup.workerLog(msgObject.data);
      } else if (tag === "error") {
        consoleSetup.workerError(msgObject.data);
      } else {
        consoleSetup.workerLog(msgObject.data);
      }
    } else {
      var msgType = msgObject["type"];
      if (msgType == "echo-log") {
        consoleSetup.workerLog(msgObject.contents);
      } else if (msgType == "echo-err") {
        consoleSetup.workerError(msgObject.contents);
      } else if (msgType == "compile-failure") {
        consoleSetup.workerError("Compilation failure");
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
          try {
            const start = window.performance.now();
            const result = runner.makeRequire("/compiled/project")("program.arr.js");
            const end = window.performance.now();
            console.log("Run complete with: ", result);
            printTimes();
          } catch (err) {
            consoleSetup.workerError(err);
          }
        } else if (runChoice === 'async') {
          runChoice = 'none';
          const entry = runner.makeRequireAsync("/compiled/project");
          const resultP = entry("program.arr.js");
          resultP.catch((e) => { console.log("Run failed with: ", e); printTimes(); });
          resultP.then((r) => { console.log("Run complete with: " , r); printTimes(); });
        }
      } else {
        consoleSetup.workerLog(e.data);
      }

    }
  } catch(error) {
    consoleSetup.workerLog("Error occurred: ", error, e.data);
  }
};

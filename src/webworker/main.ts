const myWorker = new Worker("pyret.jarr");
const projectsDir = "/projects";
const prewritten = "/prewritten";
const uncompiled = "/uncompiled";

const consoleSetup = require("./console-setup.ts");

const bfsSetup = require("./browserfs-setup.ts");
bfsSetup.install();
bfsSetup.configure(myWorker);

const backend = require("./backend.ts");

const runner = require("./runner.ts")(bfsSetup.fs, bfsSetup.path);
const pyretApi = require("./pyret-api.ts");

const loader = require("./runtime-loader.ts");
const worker = myWorker;

const input = <HTMLInputElement>document.getElementById("program");
const compile = document.getElementById("compile");
const compileRun = document.getElementById("compileRun");
const compileRunStopify = document.getElementById("compileRunStopify");
const typeCheckBox = <HTMLInputElement>document.getElementById("typeCheck");

const showBFS = <HTMLInputElement>document.getElementById("showBFS");

const FilesystemBrowser = require("./filesystemBrowser.ts");
const filesystemBrowser = document.getElementById('filesystemBrowser');
FilesystemBrowser.createBrowser(bfsSetup.fs, "/", filesystemBrowser);

const NO_RUNS = "none";
var runChoice = NO_RUNS;

const myProgram = "program.arr";
const baseDir = "/projects";
const builtinJSDir = "/prewritten/";
const checks = "none";

const thePath = bfsSetup.path;

function deleteDir(dir) {
  // console.log("Entering:", dir);
  bfsSetup.fs.readdir(dir, function(err, files) {
    if (err) {
      throw err;
    }

    let count = files.length;
    files.forEach(function(file) {
      let filePath = thePath.join(dir, file);
      
      bfsSetup.fs.stat(filePath, function(err, stats) {
        if (err) {
          throw err;
        }

        if (stats.isDirectory()) {
          deleteDir(filePath);
        } else {
          bfsSetup.fs.unlink(filePath, function(err) {
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
  loader(bfsSetup.BrowserFS, prewritten, uncompiled);
  console.log("FINISHED LOADING RUNTIME FILES");
}

const loadBuiltinsButton = document.getElementById("loadBuiltins");
loadBuiltinsButton.onclick = function() {
  loadBuiltins();
}

loadBuiltins();

function compileHelper() {
  bfsSetup.fs.writeFileSync("./projects/program.arr", input.value);
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
    backend.runProgram(runner, "/compiled/project", "program.arr.js", runChoice)
      .catch(function(error) {
        console.error("Run failed with: ", error);
      })
      .then((result) => {
        console.log("Run complete with: ", result.result);
        console.log("Run complete in: ", result.time);
      });
    runChoice = NO_RUNS;
  }
}
const backendMessageHandler = 
  backend.makeBackendMessageHandler(echoLog, echoErr, compileFailure, compileSuccess);

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
      if (backendMessageHandler(e) === null) {
        consoleSetup.workerLog("FALLEN THROUGH:", e.data);
      }
    }
  } catch(error) {
    consoleSetup.workerLog("Error occurred: ", error, e.data);
  }
};

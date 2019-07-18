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

const theFs = BrowserFS.BFSRequire("fs");

// createFileNode : string -> dom element
//
// Creates a list item for the file system viewer. If the
// item is clicked on it expands to either a list of
// files (if path refers to a directory), or the contents
// of a file (if path refers to a file).
const createFileNode = (path) => {
  const fileNode = document.createElement("li");

  theFs.stat(path, (err, stats) => {
    if (err) {
      throw err;
    }

    if (stats.isDirectory()) {
      if (path !== "/") {
        path = path + "/";
      }

      fileNode.onclick = (e) => {
        e.stopPropagation();

        if (fileNode.children.length === 0) {
          const children = document.createElement("ul");
          fileNode.appendChild(children);

          theFs.readdir(path, (err, files) => {
            if (err) {
              throw err;
            }

            files.forEach((file) => {
              const child = createFileNode(path + file);
              children.appendChild(child);
            });
          });
        } else {
          while (fileNode.firstChild) {
            fileNode.removeChild(fileNode.firstChild);
          }
          fileNode.innerHTML = path;
        }
      };
    } else if (stats.isFile()) {
      fileNode.onclick = (e) => {
        e.stopPropagation();

        if (fileNode.children.length === 0) {
          const readFileOptions = {
            encoding: "utf-8",
          };
          theFs.readFile(path, readFileOptions, (err, contents) => {
            if (err) {
              throw err;
            }

            const overflowDiv = document.createElement("div");
            fileNode.appendChild(overflowDiv);
            overflowDiv.style["overflow-y"] = "scroll";
            overflowDiv.style["max-height"] = "200px";
            overflowDiv.style["border"] = "2px solid black";
            overflowDiv.style["padding"] = "5px";
            const codeElement = document.createElement("p");
            overflowDiv.appendChild(codeElement);
            codeElement.style["white-space"] = "pre-wrap";
            codeElement.innerHTML = contents;
          });
        } else {
          while (fileNode.firstChild) {
            fileNode.removeChild(fileNode.firstChild);
          }
          fileNode.innerHTML = path;
        }
      };
    }
  });

  fileNode.innerHTML = path;

  return fileNode;
};

// create root node for the file system browser.
const filesystemBrowser = document.getElementById('filesystemBrowser');
filesystemBrowser.appendChild(createFileNode("/"));

var styler = document.createElement('style');
styler.type = 'text/css';
styler.innerHTML = 
  ".log { color: #000; }" +
  ".errorLog { background-color: rgb(250, 230, 230); color: crimson }"
;
document.getElementsByTagName('head')[0].appendChild(styler);

// Setup HTML output
// NOTE(alex): May need to CTRL + F5 (refresh and clear cache) in order to see HTML logs
var consoleOutputElement = document.getElementById("consoleOut");
var outputList = document.createElement("ul");
consoleOutputElement.appendChild(outputList);
const oldLog = console.log;
const oldError = console.error;

const clearLogsButton = document.getElementById("clearLogs");
clearLogsButton.onclick = function() {
  outputList.innerHTML = "";
}

const genericLog = function(prefix, className, ...args: any[]) {
  var outputLine = prefix;
  let logArgs = args[0];

  for (let i = 0; i < logArgs.length; i++) {
    var separator = " ";
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
   
  
  var li = document.createElement("li");
  li.innerHTML = outputLine;
  li.className = className;
  outputList.appendChild(li);

  consoleOutputElement.scrollTop = consoleOutputElement.scrollHeight;
};

console.log = function(...args) {
  genericLog("[LOG]", "log", args);
  oldLog.apply(console, args);
}

console.error = function(...args) {
  genericLog("[ERR]", "errorLog", args);
  oldError.apply(console, args);
}

const workerLog = function(...args) {
  genericLog("[WORKER]", "log", args);
  args.unshift("Worker:");
  oldLog.apply(console, args);
};

const workerError = function(...args) {
  genericLog("[WORKER-ERR]", "errorLog", args);
  args.unshift("Worker Error:");
  oldError.apply(console, args);
};

window["BrowserFS"] = BrowserFS;
module.exports = {
  BrowserFS: BrowserFS,
  worker: myWorker,
  workerLog: workerLog,
  workerError: workerError
};

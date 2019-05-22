var BrowserFS = require("browserfs");

var myWorker = new Worker('pyret.jarr');
let input = <HTMLInputElement>document.getElementById("program");
let compile = document.getElementById("compile");

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

  var headElement = document.getElementsByTagName("HEAD")[0];
  var scriptElement = document.createElement('script');
  scriptElement.type = 'text/javascript';
  scriptElement.src = "pyret-api.js";
  headElement.appendChild(scriptElement);
  
  if (e) {
    throw e;
  }
});

compile.onclick = function() {
  let message = {
    program: input.value,
    options: {
      program: "./program",
      "base-dir": "./",
      checks: "none",
    }
  };
  myWorker.postMessage(JSON.stringify(message));
  console.log('Message posted to worker');
};

myWorker.onmessage = function(e) {
  console.log("Message from worker: ", e);
};

window["BrowserFS"] = BrowserFS;

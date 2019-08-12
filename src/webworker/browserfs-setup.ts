const BrowserFS = require("browserfs");
window["BrowserFS"] = BrowserFS;

const loader = require("./runtime-loader.ts");

const myWorker = new Worker('pyret.jarr');

window["projectsDir"] = "./projects";

function loadBuiltins() {
  console.log("LOADING RUNTIME FILES");
  loader();
  console.log("FINISHED LOADING RUNTIME FILES");
}

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

loadBuiltins();
module.exports = {
  BrowserFS: BrowserFS,
  worker: myWorker,
};

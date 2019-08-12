const BrowserFS = require("browserfs");
window["BrowserFS"] = BrowserFS;

const FilesystemBrowser = require("./filesystemBrowser.ts");
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

const theFS = BrowserFS.BFSRequire("fs");
const thePath = BrowserFS.BFSRequire("path");

function deleteDir(dir) {
  // console.log("Entering:", dir);
  theFS.readdir(dir, function(err, files) {
    if (err) {
      throw err;
    }

    let count = files.length;
    files.forEach(function(file) {
      let filePath = thePath.join(dir, file);
      
      theFS.stat(filePath, function(err, stats) {
        if (err) {
          throw err;
        }

        if (stats.isDirectory()) {
          deleteDir(filePath);
        } else {
          theFS.unlink(filePath, function(err) {
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


const filesystemBrowser = document.getElementById('filesystemBrowser');
FilesystemBrowser.createBrowser(theFS, "/", filesystemBrowser);

loadBuiltins();
module.exports = {
  BrowserFS: BrowserFS,
  worker: myWorker,
};

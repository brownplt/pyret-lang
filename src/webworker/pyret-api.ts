var loader = require("./runtime-loader");
var BrowserFS = require("browserfs");

BrowserFS.configure({
  fs: "LocalStorage"
}, function(e) {
  if (e) {
    throw e;
  }
});



const projectPrefix = "_project";

function getProjects() {
  let fs = BrowserFS.BFSRequire("fs");
  let files = fs.readdirSync("./");
  let projectList = [];

  files.forEach(function(file) {
    let statResult = fs.statSync(file);
    if (statResult.isDirectory()) {
      if (file.startsWith(projectPrefix)) {
        projectList.push({ file: file });   
      }
    }
  });
}

function runPath(path) {
  
}

console.log("LOADING");
loader();
console.log("FINISHED LOADING");
console.log(getProjects());
console.log("FINISHED GETTING");
